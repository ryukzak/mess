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
    gen_server:call({global, ?SERVER},
                    {add_local_task, M, F, A, undefined}).

add_local_task(M, F, A, Comment) ->
    gen_server:call({global, ?SERVER},
                    {add_local_task, M, F, A, Comment}).

get_local_task() ->
    gen_server:call({global, ?SERVER},
                    get_local_task).
    
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
handle_call(get_local_task, _From, State) ->
    Q = qlc:q([{T#local_task.m
                , T#local_task.f
                , T#local_task.a
               } || T <- mnesia:table(local_task)]),
    {atomic, MFAs} = mnesia:transaction(fun() -> qlc:eval(Q) end),
    Reply = {ok, MFAs},
    {reply, Reply, State};

handle_call({add_local_task, M, F, A, Comment}, _From, State) ->
    Q1 = qlc:q([N#node.address || N <- mnesia:table(node)]),
    Tables = try M:tables()
             catch error:_Reason -> []
             end,
    Q2 = case mnesia:table_info(tables, size) of
             0 -> qlc:q([begin 
                             mnesia:write(#tables{name=NNeed}),
                             FNeed
                         end || {NNeed, FNeed} <- Tables]);
             _ -> qlc:q([begin
                             mnesia:write(#tables{name=NNeed}),
                             FNeed
                         end || #tables{name=NExist} <- mnesia:table(tables)
                                    , {NNeed, FNeed} <- Tables
                                    , NExist /= NNeed
                                   ])
         end,
    Fun = fun() -> NextValue = util:next_value(local_task),
                   mnesia:write(#used_module{name = M}),
                   mnesia:write(#local_task{id = NextValue
                                            , m = M
                                            , f = F
                                            , a = A
                                            , comment = Comment
                                           }),
                   {qlc:eval(Q1), qlc:eval(Q2)}
          end,
    {atomic, {Nodes, TablesCreateFunction}} = mnesia:transaction(Fun),
    lists:foreach(fun(N) ->
                          slave_task_manager:add_local_task(N, M, F, A)
                  end, Nodes),
    lists:foreach(fun(FCreate) -> FCreate() end, TablesCreateFunction),
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
