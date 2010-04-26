%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 27 Apr 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(test_util).

%% API
-export([
         upload/1
        ]).

-define(EBIN,"ebin/").

%%%===================================================================
%%% API
%%%===================================================================
upload(Node) ->
    {ok,FileList} = file:list_dir(?EBIN),

    Apps = [list_to_atom(re:replace(N, ".app","", [{return,list}]))
            || N <- FileList,
               case re:run(N, ".app") of
                   {match, _} -> true;
                   _ -> false
               end], %fixme regexp

    Modules = [list_to_atom(re:replace(N, ".beam","", [{return,list}]))
               || N <- FileList,
                  case re:run(N, ".beam") of
                      {match, _} -> true;
                      _ -> false
                  end], %fixme regexp

    [send_app(Node, A) || A <- Apps],
    [send_module(Node, M) || M <- Modules].

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_app(Node, App) ->
    {ok, AppSpec} = file:script(?EBIN ++ atom_to_list(App) ++ ".app"),
    rpc:call(Node, application, load, [AppSpec]).

send_module(Node, Module) ->
    {_Module, Binary, Filename} = code:get_object_code(Module),
    rpc:call(Node, code, load_binary, [Module, Filename, Binary]).
