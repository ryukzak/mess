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
         , node_up/1
         , node_down/1
         , node_restart/1
         , send_app/2
         , send_module/2
        ]).

-define(EBIN,"/home/ryukzak/Documents/mess/ebin/").
-define(SSH,"/usr/bin/ssh").
-define(NODE_UP_TIMEOUT,50).
-define(PING_LOOP_REPEAT,40).

%%%===================================================================
%%% API
%%%===================================================================
node_up(Node) ->
    URL = re:replace(atom_to_list(Node),"^[^@]*@","",[{return,list}]),
    open_port({spawn_executable, ?SSH},
              [stream, {args, [URL, "-f",
                               "erl -detached -noinput -name \""
                               ++ atom_to_list(Node)
                               ++ "\" -setcookie \""
                               ++ atom_to_list(erlang:get_cookie())
                               ++ "\""]}]),
    ping_loop(Node, pong).

node_down(Node) ->
    rpc:call(Node,erlang,halt,[]),
    timer:sleep(?NODE_UP_TIMEOUT),
    ping_loop(Node, pang).


ping_loop(Node, Need) -> ping_loop(Node, Need, ?PING_LOOP_REPEAT).

ping_loop(_Node, _Need, 0) ->
    error;

ping_loop(Node, Need, N) ->
    timer:sleep(?NODE_UP_TIMEOUT),
    case net_adm:ping(Node) of
        Need -> ok;
        _ -> ping_loop(Node, Need, N - 1)
    end.






node_restart(Node) ->
    ok = node_down(Node),
    ok = upload(Node),
    ok = node_up(Node).
    
    

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
