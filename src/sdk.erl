-module(sdk).

-export([init/2
				 , tcp_cast/2
				 , cast/2
				 , terminate/1
				 , enviroment/0
				 , fib/1
				 , init_bool/0
				]).

-record(state,{id}).

-record(sdk,{id, pid}).
-record(data,{number, from, fib}).

init_bool() -> false.

enviroment() ->
		{atomic, ok} = mnesia:create_table(sdk, [{attributes,record_info(fields, sdk)}]),
		{atomic, ok} = mnesia:create_table(data, [{attributes,record_info(fields, data)}]),
		ok.

init(["id", IdStr], _State) ->
		Id = list_to_integer(IdStr),
		mnesia:transaction(fun() -> mnesia:write(#sdk{id = Id, pid = self()}) end),
		{true, #state{id = Id}}.

tcp_cast(["number", NumberStr], #state{id = Id} = State) ->
		Number = list_to_integer(NumberStr),
		mnesia:transaction(
			fun() -> mnesia:write(#data{number = Number
																	, from = Id
																	, fib = fib(Number)
																 }) end),
		State.

cast(Msg, State) ->
		{Msg, State}.

%% fib(1) -> 1;
%% fib(2) -> 1;
%% fib(N) ->
%% 		fib(N, 2, 1, 1).

%% fib(N, N, A, B) ->
%% 		A + B;
%% fib(N, C, A, B) ->
%% 		fib(N, C+1, B, B + A).

fib(0) -> 1;
fib(1) -> 1;
fib(2) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

terminate(#state{id = Id}) ->
		mnesia:transaction(fun() -> mnesia:delete({sdk, Id}) end),
		ok.

