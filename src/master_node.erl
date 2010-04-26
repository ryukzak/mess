%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(master_node).

-behaviour(gen_server).

%% API
-export([
         start_link/1,
         where/0,
         connect/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("tables.hrl").
-include_lib("stdlib/include/qlc.hrl").

-import(mnesia, [transaction/1]).

-define(SERVER, ?MODULE). 

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
start_link(FromNode) ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [FromNode], []).

%%--------------------------------------------------------------------
%% @doc
%% Get node address, where_is master_node.
%%
%% @spec where() -> {ok, node()} | {error, Error}
%% @end
%%--------------------------------------------------------------------
where() ->
    gen_server:call({global, ?SERVER}, where).

%%--------------------------------------------------------------------
%% @doc
%% Connect process to master node. It's should be a slave_node. Also
%% master_node monitoring slave node and conversely.
%%
%% @spec connect() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------

connect() ->
    {ok, MasterNode} = master_node:where(),
    monitor_node(MasterNode, true),
    SlaveNode = node(),
    gen_server:call({global, ?SERVER}, {connect, SlaveNode}).

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
init([undefined]) ->
    % Start cluster
    % Create system table for cluster
    create_system_table(),
    create_counter(local_task, 0),
    application:set_env(slave_node, master_node, node()),
    spawn(fun() -> application:start(slave_node) end),
    {ok, #state{}};
init([_FromNode]) ->
    % Start new master node in cluster
    Nodes = [node()|nodes()],
    % remove all disconnected node from db and get list with them
    Q1 = qlc:q([begin
                    mnesia:delete_object(N),
                    N#node.address
                end || N <- mnesia:table(node),
                       not lists:member(N#node.address, Nodes)]),
    Q2 = qlc:q([M || #used_module{name = M} <- mnesia:table(used_module)]),
    {atomic, _} = transaction(fun() ->
                                      DownNode = qlc:eval(Q1),
                                      UsedModule = qlc:eval(Q2),
                                      [catch qlc:eval(M:clean(DownNode))
                                       || M <- UsedModule]
                              end),
    mnesia:change_config(extra_db_nodes, Nodes),
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
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(where, _From, State) ->
    Reply = {ok, node()},
    {reply, Reply, State};

handle_call({connect, SlaveNode}, _From, State) ->
    monitor_node(SlaveNode, true),
    transaction(fun() -> mnesia:write(#node{address=SlaveNode}) end),

    copy_system_table(SlaveNode), % fixme

    Reply = ok,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(Msg, State) ->
    io:format("trace: ~p~n", [Msg]),
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
handle_info({nodedown, Node}, State) ->
    io:format("Nodedown: ~p~n", [Node]),
    transaction(fun() -> mnesia:delete({node, Node}) end),
    % fixme add node_depended clean
    {noreply, State};

handle_info(Info, State) ->
    io:format("Info: ~p~n", [Info]),
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

create_system_table() ->
    {atomic,ok} = 
        mnesia:create_table(node,
                            [{ram_copies, [node()]}
                             , {attributes,record_info(fields, node)}]),
    {atomic,ok} = 
        mnesia:create_table(counter,
                            [{ram_copies, [node()]}
                             , {attributes,record_info(fields, counter)}]),
    {atomic,ok} = 
        mnesia:create_table(tables,
                            [{ram_copies, [node()]}
                             , {attributes,record_info(fields, tables)}]),
    {atomic,ok} = 
        mnesia:create_table(used_module,
                            [{ram_copies, [node()]}
                             , {attributes,record_info(fields,used_module)}]),
    {atomic,ok} = 
        mnesia:create_table(local_task,
                            [{ram_copies, [node()]}
                             , {attributes,record_info(fields, local_task)}]).

create_counter(Name, Value) ->
    transaction(fun() -> mnesia:write(#counter{name=Name
                                               , value=Value}) end).

copy_system_table(SlaveNode) when SlaveNode /= node() ->
    mnesia:change_config(extra_db_nodes, nodes()),
    mnesia:add_table_copy(node, SlaveNode, ram_copies),
    mnesia:add_table_copy(counter, SlaveNode, ram_copies),
    mnesia:add_table_copy(tables, SlaveNode, ram_copies),
    mnesia:add_table_copy(local_task, SlaveNode, ram_copies),
    mnesia:add_table_copy(used_module, SlaveNode, ram_copies),
    Q = qlc:q([fun() -> mnesia:add_table_copy(T#tables.name
                                              , SlaveNode
                                              , ram_copies
                                             )
               end
               || T <- mnesia:table(tables)]),
    {atomic, FuntionCopy} = mnesia:transaction(fun() -> qlc:eval(Q) end),
    lists:foreach(fun(F) -> F() end, FuntionCopy);

copy_system_table(_) -> ok.
