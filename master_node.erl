%%%----------------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 28 Mar 2010 by Ryukzak Neskazov <>
%%%----------------------------------------------------------------------------
-module(master_node).

-behaviour(gen_server).

%% API
-export([
				 start_link/0
				 , attach/1
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
		gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

attach(Node) ->
		gen_server:call({global, ?SERVER}, {attach, Node}).

ping() ->
		gen_server:call({global, ?SERVER}, ping).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

init([]) ->
		slave_node:start_link(node()),
		{ok, #state{}}.



handle_call({attach, Node}, _From, State) ->
		case net_adm:ping(Node) of
				pong -> spawn(Node, fun() -> slave_node:start_link() end),
								{reply, ok, State};
				pang -> {reply, can_not_connect, State}
		end;

handle_call(ping, _From, State) -> {reply, pong, State};

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
