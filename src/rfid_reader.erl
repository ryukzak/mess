-module(rfid_reader).

-export([init/2
				 , tcp_cast/2
				 , cast/2
				 , terminate/1
				 , enviroment/0
				 , fib/1
				 , init_bool/0
				]).

-record(state,{id
							 , where_request = []
							 , x
							 , y
							}).

-record(rfid_reader,{id, pid}).
-record(mark_info,{mid, id}).

init_bool() -> true.

enviroment() ->
		{atomic, ok} = mnesia:create_table(sdk, [{attributes,record_info(fields, rfid_reader)}]),
		{atomic, ok} = mnesia:create_table(data, [{attributes,record_info(fields, mark_info)}
																							, {type, bag}
																						 ]),
		ok.



init(["id", IDstr, Xstr, Ystr], _State) ->
		ID = list_to_integer(IDstr),
		X = list_to_integer(Xstr),
		Y = list_to_integer(Ystr),
		mnesia:transaction(fun() -> mnesia:write(#rfid_reader{id = ID,
																													pid = self()
																												 }) end),
		{false, #state{id = ID, x = X, y = Y}}.



tcp_cast(["here", MIDstr], #state{id = Id} = State) ->
		MID = list_to_integer(MIDstr),
		mnesia:transaction(
			fun() -> mnesia:write(#mark_info{mid = MID
																			 , id = Id
																			}) end),
		State;

tcp_cast(["unhere", MIDstr], #state{id = Id} = State) ->
		MID = list_to_integer(MIDstr),		
		mnesia:transaction(
			fun() -> mnesia:delete(#mark_info{mid = MID
																				, id = Id
																			 }) end),
		State;

tcp_cast(["radius", MIDStr, RadStr], #state{x=X, y=Y,
																						where_request=WhereRequest
																					 } = State) ->
		MID = list_to_integer(MIDStr),
		Rad = list_to_integer(RadStr),
		Waits = wait_mid(WhereRequest, MID),
		lists:map(fun(p) -> p ! {radius, MID, Rad, X, Y} end, Waits),
		State.



cast({where, MID, Pid}, #state{where_request=WhereRequest} = State) ->
		case wait_mid(WhereRequest, MID) of
				[] ->
						Msg = "where " ++ erlang:integer_to_list(MID, 10) ++ "\n",
						{Msg, State#state{where_request = [{MID, Pid} | WhereRequest]}};
				_ ->
						{none, State#state{where_request = [{MID, Pid} | WhereRequest]}}
		end.

wait_mid(WhereRequest, MID) ->
		lists:filter(fun({M, _}) when M == MID -> true;
										(_) -> false
								 end, WhereRequest).

		

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

