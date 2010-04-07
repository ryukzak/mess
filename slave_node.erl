%%%----------------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 28 Mar 2010 by Ryukzak Neskazov <>
%%%----------------------------------------------------------------------------
-module(slave_node).

-behaviour(gen_server).

%% API
-export([
				 start_link/0
				 , start_link/1
				 , ping/0
				]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
				 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%============================================================================
%%% API
%%%============================================================================

start_link() ->
		gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(MasterNode) ->
		gen_server:start_link({local, ?SERVER}, ?MODULE, [MasterNode], []).

ping() ->
		gen_server:call(?SERVER, ping).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

init([]) ->
		{ok, #state{}};

init([MasterNode]) ->
		monitor_node(MasterNode, true),
		printer:format("init slave_node~n"),
		{ok, #state{}}.



handle_call(ping, _From, State) ->
		printer:format("ping slave_node~n"),
		{reply, pong, State};

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

%%%============================================================================
%%% Internal functions
%%%============================================================================
