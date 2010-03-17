-module(sdk).

-export([init/2
				 , tcp_cast/2
				 , cast/2
				 , terminate/1
				 , fib/1
				]).

-record(state,{id}).

-record(sdk,{id, pid}).
-record(data,{number, from, fib}).

init(["id", IdStr], _State) ->
		case mnesia:create_table(sdk, [{attributes,record_info(fields, sdk)}]) of				
				{atomic,ok} -> ok;
				{aborted,{already_exists,sdk}} -> ok
		end,
		case mnesia:create_table(data, [{attributes,record_info(fields, data)}]) of
				{atomic,ok} -> ok;
				{aborted,{already_exists,data}} -> ok
		end,
		Id = list_to_integer(IdStr),
		mnesia:transaction(
			fun() -> mnesia:write(#sdk{id = Id, pid = self()}) end),
%% 		io:format("Init: ~p~n", [Id]),
		{true, #state{id = Id}}.

tcp_cast(["number", NumberStr], #state{id = Id} = State) ->
		Number = list_to_integer(NumberStr),
%%  		io:format("tcp_cast: ~p <- ~p~n", [Id, Number]),
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
%% 		io:format("terminate: ~p~n", [Id]),
		mnesia:transaction(
			fun() -> mnesia:delete({sdk, Id}) end),
%%  		io:format(">~p terminate~n", [self()]),
		ok.

