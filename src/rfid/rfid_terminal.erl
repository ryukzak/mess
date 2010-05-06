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

-import(lists, [map/2, keyfind/3, keydelete/3, keyreplace/4]).

-record(state,{request}).

-record(request,{mid, count}).
-define(TIMEOUT,1000).
-include("rfid_head.hrl").

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
    map(fun (X) ->
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
        ++ integer_to_list(Mid) ++ " "
        ++ integer_to_list(Rad) ++ " "
        ++ integer_to_list(X) ++ " "
        ++ integer_to_list(Y) ++ "\n",
    {_, Count} = keyfind(Mid, 1, Request),
    case Count of
        1 -> {Msg ++ "end " ++ integer_to_list(Mid) ++ "\n",
              State#state{request = keydelete(Mid, 1, Request)}};
        _ -> {Msg, State#state{ request = keyreplace(Mid,
                                                           1,
                                                           Request,
                                                           {Mid, Count - 1})}}
    end;

cast({timeout, Mid}, #state{request=Request} = State) ->
    {"end " ++ integer_to_list(Mid) ++ "\n",
     State#state{request = keydelete(Mid, 1, Request)}}.

%%------------------------------------------------------------------------------

terminate(_State) ->
    ok.