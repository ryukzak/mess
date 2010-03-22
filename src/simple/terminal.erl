-module(terminal).

-export([init/2
         , tcp_cast/2
         , terminate/1
         , cast/2
        ]).

init(Msg, State) ->	
    io:format("> ~p get init msg: ~p, state: ~p~n", [self(), Msg, State]),
    {true, State}.

tcp_cast(["number", NStr], State) ->
    N = list_to_integer(NStr),
    X = mnesia:dirty_read({data, N}),
    [{_, _, _, NRes}] = X,
    chanel:cast(self(), {number, NRes}),
    State.

cast({number, N}, State) ->
    TCPMsg = integer_to_list(N) ++ "\n",
    {TCPMsg, State}.

terminate(_State) ->
    io:format("> ~p terminate~n", [self()]),
    ok.



