%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created :  2 Mar 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(listener).

-export([start/3
         , stop/1
         , start_worker/2
        ]).

-record(state,{name, lsock, mod}).

%%%===================================================================
%%% API
%%%===================================================================

start(Name, Port, Mod) ->
    io:format("Listener start link: ~p~n", [{Name, Port, Mod}]),
    {ok, LSock} = gen_tcp:listen(Port, [list, {active, false}]),
    Pid = spawn(fun () -> register(Name,self()),
                          loop(#state{lsock = LSock, name = Name, mod = Mod}) end),
    {ok, Pid}.

stop(Name) ->
    Name ! stop.

loop(#state{lsock = LSock, name = _Name, mod = Mod} = State) ->
    case gen_tcp:accept(LSock, 500) of
        {ok, Sock} ->	Pid = start_worker(Sock, Mod),
%% 											io:format("~p~n", [Pid]),
                      unlink(Pid),
                      gen_tcp:controlling_process(Sock, Pid),
                      inet:setopts(Sock, [{active, true}]);
        {error, timeout} -> ok;
        {error, Reason} -> io:format("accept error: ~p~n", [Reason])
    end,
    receive
        stop -> gen_tcp:close(LSock),
                io:format("Listener was stopped~n");
        X -> io:format("Listener loop: ~p~n", [X]),
             loop(State)
    after
        0 -> loop(State)
    end.

start_worker(Sock, Mod) ->
%% 		io:format("Listener spawn: ~p~n", [Mod]),
    {ok, Pid} = chanel:start_link(Sock, Mod),
    Pid.

%% 		io:format("Cat init start sock: ~p~n", [Mod]),
%% 		cat().

%% cat() ->
%% 		receive
%% 				X -> io:format("Cat: ~p~n", [X])
%% 		end,
%% 		cat().
