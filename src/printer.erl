%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created :  7 Apr 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(printer).

-behaviour(gen_server).

%% API
-export([
         start_link/0
         , format/2
         , format/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

format(Msg) ->
    gen_server:call(?SERVER, {format, Msg}).

format(Msg, List) ->
    gen_server:call(?SERVER, {format, Msg, List}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.



handle_call({format, Msg}, _From, State) ->
    io:format(Msg),
    {reply, ok, State};

handle_call({format, Msg, List}, _From, State) ->
    io:format(Msg, List),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info(_Info, State) ->
    {noreply, State}.



terminate(_Reason, _State) ->
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================
