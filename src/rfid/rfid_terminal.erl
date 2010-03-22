-module(rfid_terminal).

-export([init/2
         , tcp_cast/2
         , cast/2
         , terminate/1
         , enviroment/0
         , enviroment_name/0
         , init_bool/0
         , timeout/2
        ]).

-record(state,{request}).
-record(request,{mid, count}).

-define(TIMEOUT,1000).

-record(rfid_reader,{id, pid}).
-record(mark_info,{mid, id, pid}).

%%------------------------------------------------------------------------------

init_bool() -> false.

%%------------------------------------------------------------------------------

enviroment_name() ->
    rfid.

%%------------------------------------------------------------------------------

enviroment() ->
		{atomic, ok} =
        mnesia:create_table(rfid_reader,
                            [{attributes, record_info(fields, rfid_reader)}]),
		{atomic, ok} =
        mnesia:create_table(mark_info,
                            [{attributes,record_info(fields, mark_info)}
                             , {type, bag}
                            ]),
		ok.

%%------------------------------------------------------------------------------

init(_Msg, _State) ->
    ok.

%%------------------------------------------------------------------------------

tcp_cast(["where", MidS], #state{request=Request} = State) ->
    Mid = list_to_integer(MidS),
    List = mnesia:transaction(
             fun() -> mnesia:read({mark_info, Mid}) end),
    lists:map(fun (X) ->
                      X#mark_info.pid ! {where, Mid, self()}
              end, List),
    spawn(?MODULE, timeout, [self(), Mid]),
    State#state{request = [#request{mid = Mid,
                                    count = length(List)}
                           | Request]}.

timeout(Pid, Mid) ->
    receive
    after ?TIMEOUT ->
            Pid ! {timeout, Mid}
    end.

%%------------------------------------------------------------------------------

cast({radius, Mid, Rad, X, Y}, #state{request=Request} = State) ->
    Msg = "radius "
        ++ erlang:integer_to_list(Mid) ++ " "
        ++ erlang:integer_to_list(Rad) ++ " "
        ++ erlang:integer_to_list(X) ++ " "
        ++ erlang:integer_to_list(Y) ++ "\n",
    {_, Count} = lists:keyfind(Mid, 1, Request),
    case Count of
        1 -> {Msg ++ "end " ++ erlang:integer_to_list(Mid) ++ "\n",
              State#state{request = lists:keydelete(Mid, 1, Request)}};
        _ -> {Msg, State#state{ request = lists:keyreplace(Mid,
                                                           1,
                                                           Request,
                                                           {Mid, Count - 1})}}
    end;

cast({timeout, Mid}, #state{request=Request} = State) ->
    {"end " ++ erlang:integer_to_list(Mid) ++ "\n",
     State#state{request = lists:keydelete(Mid, 1, Request)}}.

%%------------------------------------------------------------------------------

terminate(_State) ->
    ok.
