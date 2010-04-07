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
         , connect/1
         , ping/0
         , where/0
         , info/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {nodes}).

%%%============================================================================
%%% API
%%%============================================================================

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

attach(Node) ->
    gen_server:call({global, ?SERVER}, {attach, Node}).

connect(Node) ->
    gen_server:call({global, ?SERVER}, {connect, Node}).

ping() ->
    gen_server:call({global, ?SERVER}, ping).

info() ->
    gen_server:call({global, ?SERVER}, info).

where() ->
    gen_server:call({global, ?SERVER}, where).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

init([]) ->
    catch	slave_node:start_link(node()),
    printer:format("New master node: ~p~n", [node()]),
    {ok, #state{nodes = [node()]}}.



handle_call({attach, Node}, _From, #state{nodes=Nodes} = State) ->
    case net_adm:ping(Node) of
        pong -> spawn(Node, slave_node, start_link, [node()]),
                {reply, ok, State#state{nodes = [Node|Nodes]}};
        pang -> {reply, can_not_connect, State}
    end;

handle_call({connect, Node}, _From, #state{nodes=Nodes} = State) ->
    printer:format("Connect: ~p~n", [Node]),
    {reply, ok, State#state{nodes = [Node|Nodes]}};

handle_call(ping, _From, State) -> {reply, pong, State};
handle_call(where, _From, State) -> {reply, node(), State};
handle_call(info, _From, State) -> {reply, State, State};

handle_call(Request, _From, State) ->
    printer:format("Request: ~p~n", [Request]),
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
