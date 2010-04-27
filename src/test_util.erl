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
         , node_up/2
         , node_down/2
        ]).

-define(EBIN,"./ebin/").
-define(SSH,"/usr/bin/ssh").
-define(NODE_UP_TIMEOUT,1000).

%%%===================================================================
%%% API
%%%===================================================================
node_up(URL, Node) ->
    open_port({spawn_executable, ?SSH},
              [stream, {args, [URL, "-f",
                               "erl -detached -noinput -name "
                               ++ full_address(URL, Node)
                               ++ " -setcookie \""
                               ++ atom_to_list(erlang:get_cookie())
                               ++ "\""]}]),
    timer:sleep(?NODE_UP_TIMEOUT),
    case net_adm:ping(list_to_atom(full_address(URL, Node))) of
        pong -> ok;
        pang -> error
    end.

node_down(URL, Node) ->
    rpc:call(list_to_atom(full_address(URL, Node)),erlang,halt,[]),
    case net_adm:ping(list_to_atom(full_address(URL, Node))) of
        pong -> error;
        pang -> ok
    end.

full_address(URL, Node) -> atom_to_list(Node) ++ "@" ++ URL.
    

upload(Node) ->
    {ok,FileList} = file:list_dir(?EBIN),

    Apps = get_atom_from_file_list(".app", FileList),
    Modules = get_atom_from_file_list(".beam", FileList),

    [send_app(Node, A) || A <- Apps],
    [send_module(Node, M) || M <- Modules],
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_atom_from_file_list(Ext0, FileList) ->
    Ext1 = Ext0++[$$],
    [list_to_atom(re:replace(N, Ext1,"", [{return,list}]))
     || N <- FileList,
        case re:run(N, Ext1) of
            {match, _} -> true;
            _ -> false
        end].

send_app(Node, App) ->
    {ok, AppSpec} = file:script(?EBIN ++ atom_to_list(App) ++ ".app"),
    rpc:call(Node, application, load, [AppSpec]).

send_module(Node, Module) ->
    {_Module, Binary, Filename} = code:get_object_code(Module),
    rpc:call(Node, code, load_binary, [Module, Filename, Binary]).
