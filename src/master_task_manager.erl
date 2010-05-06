%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(master_task_manager).

-behaviour(gen_server).

%% API
-export([
         start_link/0
         , add_local_task/3
         , add_local_task/4
         , get_local_task/0
         , reset_task/0
         , add_atom_task/4
         , restart_atom_task/1
         , stop_atom_task/1
         , node_down/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include_lib("stdlib/include/qlc.hrl").
-include_lib("tables.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

add_local_task(M, F, A) ->
    add_local_task(M, F, A, undefined).

add_local_task(M, F, A, Comment) ->
    gen_server:call({global, ?SERVER},
                    {add_local_task, #local_task{mfa={M,F,A}
                                                 , comment=Comment
                                                }}).

add_atom_task(M, F, A, Option) ->
    Task = parse_atom_option(#atom_task{mfa = {M,F,A}
                                        , from = self()
                                       }, Option),
    gen_server:call({global, ?SERVER}, {add_atom_task, Task}).

restart_atom_task(Task) ->
    gen_server:cast({global, ?SERVER}, {restart_atom_task, Task}).

stop_atom_task(Task) ->
    gen_server:cast({global, ?SERVER}, {stop_atom_task, Task}).

node_down(NodeDown) ->
    io:format("master_task_manager:node_down(~p)~n",[NodeDown]),
    Ok = gen_server:cast({global, ?SERVER}, {node_down, NodeDown}),
    io:format("master_task_manager:node_down result: ~p~n",[Ok]).

reset_task() -> gen_server:call({global, ?SERVER}, reset_task).

get_local_task() -> gen_server:call({global, ?SERVER}, get_local_task).
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    io:format("Init master task manager.~n"),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop , Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_local_task, #local_task{mfa={M,_,_}
                                        } = Task}, _From, State) ->
    % get from module with this task table list.
    NecessaryTables = necessary_task_table(M),
    
    {Nodes, TablesCreateFunction} =
        mnesia_add_local_task(NecessaryTables, Task),

    create_necessary_table(TablesCreateFunction),

    Reply = [{N, slave_task_manager:add_local_task(N, Task)}
             || N <- Nodes],
    {reply, {ok, Reply}, State};

handle_call({add_atom_task, #atom_task{mfa={M,_,_}
                                      } = Task0}, _From, State) ->
    io:format("--------------------------------------------------~n"),
    io:format("master_task add_atom_task~n"),
    % get from module with this task table list.
    NecessaryTables = necessary_task_table(M),
    case get_node_to_run(Task0) of
        error -> {reply, error, State};
        Node -> {Task, TablesCreateFunction} =
                    mnesia_add_atom_task(NecessaryTables,
                                         Task0#atom_task{run_on_node = Node}),
                create_necessary_table(TablesCreateFunction),
                slave_task_manager:add_atom_task(Task),
                {reply, ok, State}
    end;

handle_call(get_local_task, _From, State) ->
    Q = qlc:q([{M, F, A
               } || #local_task{mfa={M,F,A}} <- mnesia:table(local_task)]),
    {atomic, MFAs} = mnesia:transaction(fun() -> qlc:eval(Q) end),
    Reply = {ok, MFAs},
    {reply, Reply, State};

handle_call(reset_task, _From, State) ->
    Q1 = q_get_nodes(),
    Q2 = qlc:q([N || #tables{name = N} <- mnesia:table(tables)]),
    Fun = fun() -> {qlc:eval(Q1),qlc:eval(Q2)} end,
    {atomic,{Nodes,Tables}} = mnesia:transaction(Fun),
    % clear system table
    mnesia:clear_table(tables),
    mnesia:clear_table(used_module),
    mnesia:clear_table(local_task),
    % delete user table
    [{atomic,ok} = mnesia:delete_table(T) || T <- Tables],
    % stop all atom and local task on each node
    [slave_task_manager:reset_task(N) || N <- Nodes],
    Reply = ok,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

q_get_nodes() ->
    qlc:q([N#node.address || N <- mnesia:table(node)]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({node_down, NodeDown}, State) ->
    io:format("master_task_manager:node_down ~p~n",[NodeDown]),
    Q = qlc:q([master_task_manager:restart_atom_task(T)
               || #atom_task{run_on_node = N } = T
                      <- mnesia:table(atom_task),
                  lists:member(N, NodeDown)]),
    {atomic,_} = mnesia:transaction(fun() -> qlc:eval(Q) end),
    {noreply, State};

handle_cast({restart_atom_task, #atom_task{history=History
                                          } = Task}, State) ->
    io:format("master_task restart_atom_task~n"),
    Node = get_node_to_run(Task),
    {T,M,S} = now(),
    Now = round(timer:hms(T,M,S)/1000),
    Task1 =  Task#atom_task{history = [Now | History]
                            , run_on_node = Node
                           },
    Fun = fun() -> mnesia:write(Task1) end,
    {atomic, _} = mnesia:transaction(Fun),
    slave_task_manager:add_atom_task(Task1),
    {noreply, State};

handle_cast({stop_atom_task, #atom_task{id=Id
                                       } = _Task}, State) ->
    io:format("master_task stop_atom_task~n"),
    Fun = fun() -> mnesia:delete({atom_task, Id}) end,
    {atomic,ok} = mnesia:transaction(Fun),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



necessary_task_table(M) ->
    try M:tables()
    catch error:_Reason -> []
    end.



create_necessary_table(TablesCreateFunction) ->
    [FCreate() || FCreate <- TablesCreateFunction].



mnesia_add_local_task(NecessaryTables, #local_task{mfa={M,_,_}
                                                  } = Task) ->
    Q1 = q_get_nodes(),
    Q2 = q_create_necessary_table_function(NecessaryTables),
    Fun = fun() -> NextValue = util:next_value(local_task),
                   mnesia:write(#used_module{name = M}),
                   mnesia:write(Task#local_task{id = NextValue}),
                   {qlc:eval(Q1), qlc:eval(Q2)}
          end,
    {atomic, {Nodes, TablesCreateFunction}} = mnesia:transaction(Fun),
    {Nodes, TablesCreateFunction}.



mnesia_add_atom_task(NecessaryTables, #atom_task{mfa={M,_,_}} = Task0) ->
    Q = q_create_necessary_table_function(NecessaryTables),
    Fun = fun() -> NextValue = util:next_value(atom_task),
                   Task = Task0#atom_task{id = NextValue},
                   mnesia:write(#used_module{name = M}),
                   mnesia:write(Task),
                   {Task, qlc:eval(Q)}
          end,
    {atomic, {Task, TablesCreateFunction}} = mnesia:transaction(Fun),
    {Task, TablesCreateFunction}.


q_create_necessary_table_function(NecessaryTables) ->
    case mnesia:table_info(tables, size) of
        0 -> qlc:q([begin 
                        mnesia:write(#tables{name=NNeed}),
                        FNeed
                    end || {NNeed, FNeed} <- NecessaryTables]);
        _ -> qlc:q([begin
                        mnesia:write(#tables{name=NNeed}),
                        FNeed
                    end || #tables{name=NExist} <- mnesia:table(tables)
                               , {NNeed, FNeed} <- NecessaryTables
                               , NExist /= NNeed
                              ])
    end.



parse_atom_option(T, [O|Os]) ->
    T2 = case O of
             {comment, Comment} -> T#atom_task{comment = Comment};
             {link, Value} -> T#atom_task{link = Value};
             {type, Value} -> T#atom_task{type = Value};
             {node, Value} -> T#atom_task{node = Value};
             {restart, Value} -> T#atom_task{restart = Value}
         end,
    parse_atom_option(T2, Os);

parse_atom_option(T, []) -> T.


get_node_to_run(#atom_task{node=Node}) ->
    % fixme Node may be disconnected.
    case Node of
        undefined -> pool:get_node();
        _ -> {atomic, Nodes} =
                 mnesia:transaction(fun() -> qlc:eval(q_get_nodes()) end),
             case lists:member(Node, Nodes) of
                 true -> Node;
                 false -> error
             end
    end.
