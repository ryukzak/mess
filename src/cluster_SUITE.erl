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

-define(SLEEP,timer:sleep(?config(timeout,Config))).

-define(N1,'node1@192.168.1.19').
-define(N2,'node2@192.168.1.19').
-define(N3,'node3@192.168.1.19').
-define(N4,'node4@192.168.1.19').

-import(test_util, [
                    node_up/1
                    , node_down/1
                    , node_restart/1
                    , upload/1
                   ]).

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
    [{timeout, 100} | Config].

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




info() -> {where,       master_node:i_where(),
           tables,      master_node:i_tables(),
           nodes,       master_node:i_nodes(),
           local_task,  master_node:i_local_task(),
           counter,     master_node:i_counter(),
           used_module, master_node:i_used_module()
          }.
