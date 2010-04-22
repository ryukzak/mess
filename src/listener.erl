%%%-----------------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created :  2 Mar 2010 by Ryukzak Neskazov <>
%%%-----------------------------------------------------------------------------
-module(listener).

-export([start/3
         , stop/1
         , start_worker/2 %% not use in other module
        ]).

-record(state,{name, lsock, mod}).

%%%=============================================================================
%%% API
%%%=============================================================================

start(Name, Port, Mod) ->
    error_logger:info_msg("Listener start link: ~p~n", [{Name, Port, Mod}]),
    {ok, LSock} = gen_tcp:listen(Port, [list, {active, false}]),
    Pid = spawn(fun () -> register(Name,self()),
                          loop(#state{lsock = LSock, name = Name, mod = Mod})
                end),
    {ok, Pid}.

stop(Name) ->
    Name ! stop.

%%%=============================================================================
%%% Internal
%%%=============================================================================

loop(#state{lsock = LSock, name = _Name, mod = Mod} = State) ->
    case gen_tcp:accept(LSock, 500) of
        {ok, Sock} ->	Pid = start_worker(Sock, Mod),
                      unlink(Pid),
                      gen_tcp:controlling_process(Sock, Pid),
                      inet:setopts(Sock, [{active, true}]);
        {error, timeout} -> ok;
        {error, Reason} ->
            error_logger:error_msg("Listener accept error: ~p", [Reason])
    end,
    receive
        stop -> gen_tcp:close(LSock),
                error_logger:info_msg("Listener was stopped~n");
        X -> error_logger:warning_msg("Clerk listener get unknown message: ~p",
                                      [X]),
             loop(State)
    after
        0 -> loop(State)
    end.

start_worker(Sock, Mod) ->
    {ok, Pid} = chanel:start_link(Sock, Mod),
    Pid.
