-module(rfid_reader).

-export([tcp_cast/2
         , cast/2
         , tables/0
         , clean/1
         , terminate/1
        ]).

-include_lib("stdlib/include/qlc.hrl").
-import(lists, [map/2, keydelete/3]).

-record(state,{id
               , request = []
               , x
               , y
              }).

-record(request,{mid, pid}).
-include("rfid_head.hrl").

%%--------------------------------------------------------------------

tables() ->
    [{rfid_reader
      , fun() -> mnesia:create_table(rfid_reader,
                                     [{attributes,record_info(fields,
                                                              rfid_reader)}
                                     ])
        end}
     , {mark_info
        , fun() ->
                  mnesia:create_table(mark_info,
                                      [{attributes,record_info(fields,
                                                               mark_info)}
                                       , {type, bag}
                                      ])
          end}].

clean(DownNodes) ->
    Q = qlc:q([begin
                   mnesia:remove_object(R),
                   R#rfid_reader.id
               end || R <- mnesia:table(rfid_reader)
                          , lists:member(R#rfid_reader.node, DownNodes)]),
    F = fun () ->  
                Ids = qlc:eval(Q),
                qlc:eval(qlc:q([mnesia:remove_object(R)
                                || R <- mnesia:table(rfid_reader)
                                       , lists:member(R#mark_info.id, Ids)]))
        end,
    {atomic, _} = mnesia:transaction(F),
    ok.

%%--------------------------------------------------------------------

tcp_cast(["found", MidS], #state{id=Id} = State) ->
    Mid = list_to_integer(MidS),
    mnesia:transaction(
      fun() -> mnesia:write(#mark_info{mid = Mid
                                       , id = Id
                                       , pid = self()
                                      }) end),
    State;

tcp_cast(["lost", MidS], #state{id=Id} = State) ->
    Mid = list_to_integer(MidS),
    io:format("Delete: ~p~n", [#mark_info{mid = Mid
                                        , id = Id
                                        , pid = self()
                                       }]),
    mnesia:transaction(
      fun() -> mnesia:delete_object(#mark_info{mid = Mid
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
    State#state{request = keydelete(Mid, 1, Request)};

% execute only when you first call
tcp_cast(["id", IdS, Xs, Ys], undefined) ->
    Id = list_to_integer(IdS),
    X = list_to_integer(Xs),
    Y = list_to_integer(Ys),
    mnesia:transaction(fun() -> mnesia:write(#rfid_reader{id = Id
                                                          , pid = self()
                                                          , node = node()
                                                          , x = X
                                                          , y = Y
                                                         })
                       end),
    {ok, #state{id = Id, x = X, y = Y}}.

%%--------------------------------------------------------------------

cast({where, Mid, Pid}, #state{request=Request} = State) ->
    case [X || X <- Request, X#request.mid /= Mid] of
        [] -> Msg = "where " ++ erlang:integer_to_list(Mid, 10) ++ "\n",
              {msg, Msg,
               State#state{request =
                           [#request{mid = Mid, pid = Pid} | Request]}};
        % request has been sent
        _ -> State#state{request =
                         [#request{mid = Mid, pid = Pid} | Request]}
    end.

%%--------------------------------------------------------------------

terminate(#state{id=Id}) ->
    Q = qlc:q([mnesia:delete_object(X) || X <- mnesia:table(mark_info),
                                          X#mark_info.id == Id]),
    {atomic, _} = mnesia:transaction(fun() ->
                                             mnesia:delete({rfid_reader, Id}),
                                             qlc:eval(Q)
                                     end).
