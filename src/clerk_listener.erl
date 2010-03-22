%%%-----------------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created :  2 Mar 2010 by Ryukzak Neskazov <>
%%%-----------------------------------------------------------------------------
-module(clerk_listener).

-export([
         start/1
         , stop/0
        ]).

-record(state,{name, lsock, mod}).

%%%=============================================================================
%%% API
%%%=============================================================================

start(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [list, {active, false}]),
    Pid = spawn(fun () -> register(clerk_listener,self()),
                          loop(#state{lsock = LSock}) end),
    {ok, Pid}.

stop() ->
    clerk_listener ! stop.

%%%=============================================================================
%%% Internal
%%%=============================================================================

loop(#state{lsock = LSock} = State) ->
    case gen_tcp:accept(LSock, 500) of
        {ok, Sock} ->
            gen_tcp:send(Sock, wizard:get_node_ip()),
            gen_tcp:close(Sock);				
        {error, timeout} -> ok;
        {error, Reason} ->
            error_logger:info_msg("Clerk listener socket error reason: ~p",
                                  [Reason])
    end,
    receive
        stop -> gen_tcp:close(LSock),
                error_logger:info_msg("Clerk listener stoped.");
        X -> error_logger:warning_msg("Clerk listener get unknown message: ~p",
                                      [X]),
             loop(State)
    after
        0 -> loop(State)
    end.

