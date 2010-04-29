%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 26 Apr 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(ping_pong_log_SUITE).

-suite_defaults([{timetrap, {minutes, 10}}]).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/qlc.hrl").

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
    application:start(mnesia),
    {ok, Pid} = ping_pong_log:start_link(),
    [{Name, Fun}] = ping_pong_log:tables(),
    unlink(Pid),
    [{table_name,Name}, {create_table,Fun} | Config].

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
    application:stop(mnesia),
    true = exit(whereis(ping_pong_log), kill),
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
    Fun = ?config(create_table, Config),
    {atomic, ok} = Fun(),
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
end_per_testcase(_TestCase, Config) ->
    {atomic, ok} = mnesia:delete_table(?config(table_name,Config)),
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
    [ping, ping_on_node].

%% Test cases starts here.
%%--------------------------------------------------------------------
ping() ->
    [{doc, "Ping call"}].

ping(Config) when is_list(Config) ->
    pong = ping_pong_log:ping(),
    pong = ping_pong_log:ping(),
    Name = ?config(table_name, Config),
    Node = node(),
    {atomic, [{Name,_,Node,Node}, {Name,_,Node,Node}]} =
        mnesia:transaction(
          fun() -> qlc:eval(qlc:q([R || R <- mnesia:table(Name)])) end).

ping_on_node() ->
    [{doc, "Ping call to node"}]. %fixme english

ping_on_node(Config) when is_list(Config) ->
    pong = ping_pong_log:ping(node()),
    pong = ping_pong_log:ping(node()),
    pong = ping_pong_log:ping(node()),
    Name = ?config(table_name, Config),
    Node = node(),
    {atomic, [{Name,_,Node,Node}, {Name,_,Node,Node}
              , {Name,_,Node,Node}]} =
        mnesia:transaction(
          fun() -> qlc:eval(qlc:q([R || R <- mnesia:table(Name)])) end).
