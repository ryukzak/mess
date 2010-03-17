%%%-------------------------------------------------------------------
%%% @author ryukzak <>
%%% @copyright (C) 2010, ryukzak
%%% @doc
%%%
%%% @end
%%% Created :  3 Mar 2010 by ryukzak <>
%%%-------------------------------------------------------------------
-module(chanel).

-behaviour(gen_server).

%% API
-export([start_link/2
				 , parse/3
				 , cast/2
				]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
				 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {sock, mod, init, buffer = [], chanel_state = undefined}).

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
start_link(Sock, Mod) ->
%% 		io:format("Chanel start link: ~p~n", [{Sock, Mod}]),
		gen_server:start_link(?MODULE, [Sock, Mod], []).

cast(Pid, Msg) ->
		gen_server:cast(Pid, {cast, Msg}).

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
init([Sock, Mod]) ->
		{ok, #state{sock = Sock, mod = Mod, init = false}}.

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
handle_cast({cast, Msg}, #state{mod = Mod
																, chanel_state = CS
																, sock = Sock
															 } = State) ->
		{TCPMsg, CS0} = Mod:cast(Msg, CS),
		gen_tcp:send(Sock, TCPMsg),
		{noreply, State#state{chanel_state = CS0}};
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

tcp_loop(Mod, Init, Buffer, State) ->
		case parse([], [], Buffer) of
				none -> {Init, Buffer, State};
				{Msg, [], Buffer0} ->
						case Init of
								true -> State0 = Mod:tcp_cast(Msg, State),
											  tcp_loop(Mod, true, Buffer0, State0);
								false -> {Init0, State0} = Mod:init(Msg, State),
												 tcp_loop(Mod, Init0, Buffer0, State0)
						end;
				X -> io:format("Error in chanel parser: ~p~n", [X])
		end.

%%--------------------------------------------------------------------

handle_info({tcp, _Port, Msg} = _Info, #state{buffer = Buffer
																							, init = Init
																							, chanel_state = CS
																							, mod = Mod
																						 } = State) ->
		{Init0, Buffer0, CS0} = tcp_loop(Mod, Init, Buffer ++ Msg, CS),
		{noreply, State#state{buffer = Buffer0
													, init = Init0
													, chanel_state = CS0
												 }};

handle_info({tcp_closed, _Port} = _Info, State) ->
		{stop, normal, State};

handle_info(Info, State) ->
		io:format("---------------------------------~n"),
		io:format("controller Info: ~p~n", [Info]),
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
terminate(_Reason, #state{sock = Sock
													, mod = Mod
													, chanel_state = CS
												 } = _State) ->
		case CS of
				undefined -> ok;
				X -> Mod:terminate(X)
		end,
		gen_tcp:close(Sock).

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

parse(_Acc, _Wacc, []) ->
		none;

parse(Acc, [], [10|Buffer]) ->
		{lists:reverse(Acc), [], Buffer};

parse(Acc, Wacc, [10|Buffer]) ->
		{lists:reverse([lists:reverse(Wacc) | Acc]), [], Buffer};


parse(Acc, Wacc, [13|Buffer]) -> % need to delete
		parse(Acc, Wacc, Buffer);

parse(Acc, [], [81|Buffer]) ->
		{lists:reverse(Acc), [], Buffer};
parse(Acc, Wacc, [81|Buffer]) ->
		{lists:reverse([lists:reverse(Wacc) | Acc]), [], Buffer};

parse(Acc, [], [32|Buffer]) ->
		parse(Acc, [], Buffer);

parse(Acc, Wacc, [32|Buffer]) ->
		parse([lists:reverse(Wacc) | Acc], [], Buffer);

parse(Acc, Wacc, [X|Buffer]) ->
		parse(Acc, [X | Wacc], Buffer).
