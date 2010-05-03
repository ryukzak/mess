%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 29 Apr 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(ping_pong_atom).

-behaviour(gen_server).

%% API
-export([
         start_link/0
         , start_link/1
         , ping/0
         , ping/1
         , do/2
         , rand_do/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TIMEOUT,1000).

-record(state,{function}).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(stand_alone) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [stand_alone], []).

ping() ->
    ping(node()).

ping(Node) ->
    {ok, Tag} = gen_server:call({?SERVER, Node}, ping),
    receive
        Tag -> pong;
        _ -> pang
    after
        ?TIMEOUT -> pang_timeout
    end.

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
    Fun = fun(Pid, Tag) -> master_task_manager:add_atom_task(
                             ping_pong_atom, rand_do, [Pid, Tag],
                             [{comment, "It's real erlang ping pong"}
                              , {restart, {transient, 4, 2000}}
                             ]) end,
    {ok, #state{function = Fun}};
init([stand_alone]) ->
    Fun = fun(Pid, Tag) -> spawn(ping_pong_atom, do, [Pid, Tag])
          end,
    {ok, #state{function = Fun}}.

do(From, Tag) ->
    From ! Tag.

rand_do(From, Tag) ->
    % random error
    {_,_,Micro} = now(),
    if Micro rem 10 < 4 ->
            io:format("Process error: ~p~n", Tag);
       true -> ok
    end,
    From ! Tag.
    

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
handle_call(ping, {Pid, Tag}, #state{function = Fun} = State) ->
    Fun(Pid, Tag),
    Reply = {ok, Tag},
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
