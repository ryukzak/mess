-module(echo).

-export([init/2
				 , tcp_cast/2
				 , terminate/1
				 , cast/2
				]).

init(Msg, State) ->
		io:format("> ~p get init msg: ~p~n", [self(), Msg]),
		{true, State}.

tcp_cast(Msg, State) ->		
		io:format("> ~p get do msg: ~p, state: ~p~n", [self(), Msg, State]),
		State.

cast(Msg, State) ->		
		io:format("> ~p get do msg: ~p, state: ~p~n", [self(), Msg, State]),
		TCPMsg = Msg,
		{TCPMsg, State}.

terminate(_State) ->
		io:format("> ~p terminate~n", [self()]),
		ok.


