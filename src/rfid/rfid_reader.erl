-module(rfid_reader).

-export([init/2
         , tcp_cast/2
         , cast/2
         , terminate/1
         , enviroment/0
         , enviroment_name/0
         , init_bool/0
        ]).

-record(state,{id
               , request = []
               , x
               , y
              }).

-record(request,{mid, pid}).

-record(rfid_reader,{id, pid}).
-record(mark_info,{mid, id, pid}).

%%------------------------------------------------------------------------------

init_bool() -> true.

%%------------------------------------------------------------------------------

enviroment_name() ->
    rfid.

%%------------------------------------------------------------------------------

enviroment() ->
		{atomic, ok} =
        mnesia:create_table(rfid_reader,
                            [{attributes,record_info(fields, rfid_reader)}]),
		{atomic, ok} =
        mnesia:create_table(mark_info,
                            [{attributes,record_info(fields, mark_info)}
                             , {type, bag}
                            ]),
		ok.

%%------------------------------------------------------------------------------

init(["id", IdS, Xs, Ys], _State) ->
    Id = list_to_integer(IdS),
    X = list_to_integer(Xs),
    Y = list_to_integer(Ys),
    mnesia:transaction(fun() -> mnesia:write(#rfid_reader{id = Id,
                                                          pid = self()
                                                         }) end),
    {false, #state{id = Id, x = X, y = Y}}.

%%------------------------------------------------------------------------------

tcp_cast(["here", MidS], #state{id=Id} = State) ->
    Mid = list_to_integer(MidS),
    mnesia:transaction(
      fun() -> mnesia:write(#mark_info{mid = Mid
                                       , id = Id
                                       , pid = self()
                                      }) end),
    State;

tcp_cast(["unhere", MidS], #state{id=Id} = State) ->
    Mid = list_to_integer(MidS),		
    mnesia:transaction(
      fun() -> mnesia:delete(#mark_info{mid = Mid
                                        , id = Id
                                        , pid = self()
                                       }) end),
    State;

tcp_cast(["radius", MidS, RadS], #state{x=X,
                                        y=Y,
                                        request=Request
                                       } = State) ->
    Mid = list_to_integer(MidS),
    Rad = list_to_integer(RadS),
    [Z#request.pid ! {radius, Mid, Rad, X, Y} || Z <- Request,
                                                 Z#request.mid == Mid],
    State#state{request = lists:keydelete(Mid, 1, Request)}.

%%------------------------------------------------------------------------------

cast({where, Mid, Pid}, #state{request=Request} = State) ->
    case wait_mid(Request, Mid) of
        [] ->
            Msg = "where " ++ erlang:integer_to_list(Mid, 10) ++ "\n",
            {Msg,
             State#state{request = [#request{mid = Mid, pid = Pid} | Request]}};
        _ ->
            {none,
             State#state{request = [#request{mid = Mid, pid = Pid} | Request]}}
    end.

wait_mid(WhereRequest, Mid) ->
    lists:filter(fun({M, _}) when M == Mid -> true;
                    (_) -> false
                 end, WhereRequest).

%%------------------------------------------------------------------------------

terminate(#state{id=Id}) ->
    mnesia:transaction(fun() -> mnesia:delete({rfid_reader, Id}) end),
    ok.
