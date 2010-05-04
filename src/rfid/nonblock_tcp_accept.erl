%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 24 Apr 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(nonblock_tcp_accept).

-behaviour(supervisor_bridge).

%% API
-export([
				 start_link/2
				, start_link/3
				, start_link/4
				]).

%% supervisor_bridge callbacks
-export([init/1, terminate/2]).

-define(SERVER, ?MODULE).

-define(TIMEOUT,500).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor bridge
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Pid, LSock) ->
		supervisor_bridge:start_link(?MODULE, {Pid, LSock, ?TIMEOUT}).

start_link(Pid, LSock, TimeOut)	when is_integer(TimeOut) ->
		supervisor_bridge:start_link(?MODULE, {Pid, LSock, TimeOut});

start_link(Pid, Port, Option) ->
		supervisor_bridge:start_link(?MODULE, {Pid, Port, Option, ?TIMEOUT}).

start_link(Pid, Port, Option, TimeOut) ->
		supervisor_bridge:start_link(?MODULE, {Pid, Port, Option, TimeOut}).

%%%===================================================================
%%% supervisor_bridge callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Creates a supervisor_bridge process, linked to the calling process,
%% which calls Module:init/1 to start the subsystem. To ensure a
%% synchronized start-up procedure, this function does not return
%% until Module:init/1 has returned.
%%
%% @spec init(Args) -> {ok, Pid, State} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init({Pid, Port, Option, TimeOut}) ->
    case gen_tcp:listen(Port, Option) of
				{ok, LSock} -> {ok, init_loop(Pid, LSock, TimeOut), self()};
				{error, Reason} -> {error, Reason}
		end;

init({Pid, LSock, TimeOut}) ->
		{ok, init_loop(Pid, LSock, TimeOut), self()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by the supervisor_bridge when it is about
%% to terminate. It should be the opposite of Module:init/1 and stop
%% the subsystem and do any necessary cleaning up.The return value is
%% ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, Pid) ->
		Pid ! terminate,
		ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_loop(Pid, LSock, TimeOut) ->
		spawn(fun() -> loop({Pid, LSock, TimeOut}) end).

loop({Pid, LSock, TimeOut} = State) ->
    case gen_tcp:accept(LSock, TimeOut) of
        {ok, Sock} ->	Pid ! {tcp_accept, Sock};
        {error, timeout} -> ok;
        {error, Reason} ->
            error_logger:error_msg("nonblock_tcp_accept error: ~p",
																	 [Reason])
    end,
    receive
        terminate -> gen_tcp:close(LSock);
				Msg -> error_logger:warning_msg(
								 "nonblock_tcp_accept incorrect msg: ~p",
								 [Msg]),
							 loop(State)
    after
        0 -> loop(State)
    end.
