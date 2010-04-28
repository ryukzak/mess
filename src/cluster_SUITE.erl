%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 27 Apr 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(cluster_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(EBIN,"/home/ryukzak/Documents/mess/ebin/").
-define(SSH,"/usr/bin/ssh").

-define(NODE_UP_TIMEOUT,50).
-define(PING_LOOP_REPEAT,40).
-define(SLEEP,timer:sleep(100)).

-define(N1,'node1@192.168.1.19').
-define(N2,'node2@192.168.1.19').
-define(N3,'node3@192.168.1.19').
-define(N4,'node4@192.168.1.19').

-import(ct_rpc, [call/4, cast/4]).

%% Test server callback functions
%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    [ok,ok,ok,ok] = rpc:parallel_eval([{test_util, node_up, [?N1]}
                                       , {test_util, node_up, [?N2]}
                                       , {test_util, node_up, [?N3]}
                                       , {test_util, node_up, [?N4]}
                                      ]),
    upload(?N1),
    upload(?N2),
    upload(?N3),
    upload(?N4),
    call(?N1, application, start, [master_node]),
    connect(?N2,?N1),
    connect(?N3,?N1),
    connect(?N4,?N1),
    Config.

connect(Node, MasterNode) ->
    call(Node, slave_node, connect, [MasterNode]).

%%--------------------------------------------------------------------
%% @doc
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after the whole suite
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok = node_down(?N1),
    ok = node_down(?N2),
    ok = node_down(?N3),
    ok = node_down(?N4),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Initiation before each test case
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Initiation before each test case
%%
%% @spec init_per_testcase(TestCase, Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Cleanup after each test case
%%
%% @spec end_per_testcase(TestCase, Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    rpc:call(?N1,
             master_task_manager,
             reset_task,
             []),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% TestCases - [Case]
%% Case - atom()
%%   Name of a test case.
%%
%% Returns a list of all test cases in this test suite
%%
%% @spec all() -> TestCases
%% @end
%%--------------------------------------------------------------------
all() ->
    [ping_pong].

%% Test cases starts here.
%%--------------------------------------------------------------------
ping_pong() ->
    [{doc, "Simple ping_pong test"}].

ping_pong(Config) when is_list(Config) ->
    % ?SLEEP,

    master_task_manager:add_local_task(
      ping_pong, start_link, [],"some text"),

    node_down(?N3),
    ?SLEEP,
    {ok, [_,_,_]} = master_node:i_nodes(),
    
    pong = ping_pong:ping(?N1),
    {badrpc,_} = call(?N1, ping_pong, ping, [?N3]),
    pong = call(?N1, ping_pong, ping, [?N4]),
    pong = call(?N1, ping_pong, ping, []),

    node_up(?N3),
    upload(?N3),
    connect(?N3, ?N1),

    {ok, [_,_,_,_]} = master_node:i_nodes(),
    pong = call(?N3, ping_pong, ping, []),
    
    ok.



%%%===================================================================
%%% Internal functions
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



node_restart(Node) ->
    ok = node_down(Node),
    ok = upload(Node),
    ok = node_up(Node).

%%--------------------------------------------------------------------

upload(Node) ->
    {ok,FileList} = file:list_dir(?EBIN),

    Apps = get_atom_from_file_list(".app", FileList),
    Modules = get_atom_from_file_list(".beam", FileList),

    [send_app(Node, A) || A <- Apps],
    [send_module(Node, M) || M <- Modules],
    ok.

%%--------------------------------------------------------------------

ping_loop(Node, Need) -> ping_loop(Node, Need, ?PING_LOOP_REPEAT).

ping_loop(_Node, _Need, 0) ->
    error;

ping_loop(Node, Need, N) ->
    timer:sleep(?NODE_UP_TIMEOUT),
    case net_adm:ping(Node) of
        Need -> ok;
        _ -> ping_loop(Node, Need, N - 1)
    end.



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

%%--------------------------------------------------------------------

info() -> {where,       master_node:i_where(),
           tables,      master_node:i_tables(),
           nodes,       master_node:i_nodes(),
           local_task,  master_node:i_local_task(),
           counter,     master_node:i_counter(),
           used_module, master_node:i_used_module()
          }.

