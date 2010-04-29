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
-define(SSLEEP,timer:sleep(25)).

% -define(N1,'node1@192.168.1.19').
% -define(N2,'node2@192.168.1.19').
% -define(N3,'node3@192.168.1.19').
% -define(N4,'node4@192.168.1.19').

-define(N1,'node1@127.0.0.1').
-define(N2,'node2@127.0.0.1').
-define(N3,'node3@127.0.0.1').
-define(N4,'node4@127.0.0.1').

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
    tc(fun() -> [ok,ok,ok,ok] =
                    rpc:parallel_eval([{cluster_SUITE, node_up, [?N1]}
                                       , {cluster_SUITE, node_up, [?N2]}
                                       , {cluster_SUITE, node_up, [?N3]}
                                       , {cluster_SUITE, node_up, [?N4]}
                                      ])
       end, "Node up"),
    
    tc(fun() -> upload(?N1),
                upload(?N2),
                upload(?N3),
                upload(?N4)
       end,"Node upload system"),
    
    tc(fun() -> call(?N1, application, start, [master_node])
                end,"Start master node"),

    spawn(?N1, application, start, [sasl]),
    
    ?SLEEP,
    tc(fun() -> connect(?N2,?N1),
                connect(?N3,?N1),
                connect(?N4,?N1)
       end,"Connect node"),
    Config.


tc(Fun, Str) when is_function(Fun) ->
    tc(Str, Fun);

tc(Str, Fun) ->
    Time = now(),
    Result = Fun(),
    io:format("~s: ~p sec~n",[Str,
                              timer:now_diff(now(),Time) / 1000000
                             ]),
    Result.
    


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
    tc(fun() -> ok = node_down(?N1),
                ok = node_down(?N2),
                ok = node_down(?N3),
                ok = node_down(?N4)
       end, "All node down"),
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
    tc(fun() -> rpc:call(?N1, master_task_manager,
                         reset_task,
                         [])
       end, "Reset task"),
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
all() -> [node_crash
          , ping_pong
          , ping_pong_log
          , ping_pong_node_depend_log
         ].

%%--------------------------------------------------------------------
%% Test cases starts here.
%%--------------------------------------------------------------------
node_crash() ->
    [{doc, "Crash some node, and restart it."}].

node_crash(Config) when is_list(Config) ->

    tc(fun() -> ok = node_down(?N3)
       end, "Node down"),

    tc(fun() -> ok = node_down(?N1)
       end, "Node down"),

    % need long sleep, because all slave node wait some time to
    % reconnect. It need to be optimize
    ?SLEEP, ?SLEEP, ?SLEEP, ?SLEEP, ?SLEEP,
    ?SLEEP, ?SLEEP, ?SLEEP, ?SLEEP, ?SLEEP,

    {ok, [_,_]} = master_node:i_nodes(),
    io:format("New master node: ~p~n", [master_node:i_where()]),
    
    tc(fun() -> ok = node_down(?N2)
       end, "Node down"),

    ?SLEEP, ?SLEEP, ?SLEEP, ?SLEEP, ?SLEEP,
    ?SLEEP, ?SLEEP, ?SLEEP, ?SLEEP, ?SLEEP,

    {ok, [_]} = master_node:i_nodes(),
    {ok, MasterNode} =  master_node:i_where(),
    io:format("New master node: ~p~n", [MasterNode]),
    
    tc(fun () -> ok = node_up(?N3),
                 ok = node_up(?N1),
                 ok = node_up(?N2),
                 ok = upload(?N3),
                 ok = connect(?N3, MasterNode),
                 ok = upload(?N1),
                 ok = connect(?N1, MasterNode),
                 ok = upload(?N2),
                 ok = connect(?N2, MasterNode)
       end, "New node up and connect"),
    
    {ok, [_,_,_,_]} = master_node:i_nodes(),
    
    ok.


ping_pong() ->
    [{doc, "Simple ping_pong test"}].

ping_pong(Config) when is_list(Config) ->
    ?SLEEP,
    io:format("Before add task: ~p~n", [info()]),
    
    io:format("Add task: ~p~n",
              [tc(fun() -> master_task_manager:add_local_task(
                             ping_pong, start_link, [],"some text")
                  end, "Add new task")]),

    io:format("After add task: ~p~n", [info()]),
    ?SLEEP,
    io:format("After add task sleep: ~p~n", [info()]),

    io:format("~p supervisor: ~p~n",
              [?N1, call(?N1, local_task_sup,info, [])]),
    io:format("~p supervisor: ~p~n",
              [?N2, call(?N2, local_task_sup,info, [])]),
    io:format("~p supervisor: ~p~n",
              [?N3, call(?N3, local_task_sup,info, [])]),
    io:format("~p supervisor: ~p~n",
              [?N4, call(?N4, local_task_sup,info, [])]),

    io:format("~p~n", [ping_pong:ping(?N1)]),
    io:format("~p~n", [ping_pong:ping(?N2)]),
    io:format("~p~n", [ping_pong:ping(?N3)]),
    io:format("~p~n", [ping_pong:ping(?N4)]),
    io:format("==================~n"),
    
    tc(fun() -> pong = ping_pong:ping(?N3)
       end, "Ping pong"),

    tc(fun() -> ok = node_down(?N3)
       end, "Node down"),

    % need for get message about node down (node down call guarantees
    % only that the node does not answer)
    ?SLEEP,

    {ok, [_,_,_]} = master_node:i_nodes(),

    tc(fun() -> pong = ping_pong:ping(?N1),
                {badrpc,_} = call(?N1, ping_pong, ping, [?N3]),
                pong = call(?N1, ping_pong, ping, [?N4]),
                pong = call(?N1, ping_pong, ping, [])
       end, "Ping pong 3 time correct. 1 time bad"),

    tc(fun () -> ok = node_up(?N3),
                 ok = upload(?N3),
                 ok = connect(?N3, ?N1)
       end, "New node up and connect"),
    
    {ok, [_,_,_,_]} = master_node:i_nodes(),
    pong = call(?N3, ping_pong, ping, []),
    
    ok.



ping_pong_log() ->
    [{doc, "Simple ping_pong_log test"}].

ping_pong_log(Config) when is_list(Config) ->
    io:format("Add task: ~p~n",
              [tc(fun() -> master_task_manager:add_local_task(
                             ping_pong_log, start_link, [],"some text")
                  end, "Add new task")]),
    % tc(fun() -> master_task_manager:add_local_task(
    %               ping_pong_log, start_link, [],"some text")
    %    end, "Add new task"),

    ?SLEEP,

    tc(fun() -> pong = ping_pong_log:ping(?N3)
       end, "Ping pong"),
    ?SSLEEP,
    {ok, 1} = master_node:i_get_table_size(ping_pong_log),
    
    tc(fun() -> ok = node_down(?N3)
       end, "Node down"),

    % need for get message about node down (node down call guarantees
    % only that the node does not answer)
    ?SLEEP,

    {ok, [_,_,_]} = master_node:i_nodes(),

    tc(fun() -> pong = ping_pong_log:ping(?N1),
                {badrpc,_} = call(?N1, ping_pong_log, ping, [?N3]),
                pong = call(?N1, ping_pong_log, ping, [?N4]),
                pong = call(?N1, ping_pong_log, ping, [])
       end, "Ping pong 3 time correct. 1 time bad"),
    ?SSLEEP,
    {ok, 4} = master_node:i_get_table_size(ping_pong_log),

    tc(fun () -> ok = node_up(?N3),
                 ok = upload(?N3),
                 ok = connect(?N3, ?N1)
       end, "New node up and connect"),

    {ok, [_,_,_,_]} = master_node:i_nodes(),
    pong = call(?N3, ping_pong_log, ping, []),
    ?SSLEEP,
    {ok, 5} = master_node:i_get_table_size(ping_pong_log),
    ok.



ping_pong_node_depend_log() ->
    [{doc, "Simple ping_pong_log test"}].

ping_pong_node_depend_log(Config) when is_list(Config) ->
    io:format("Add task: ~p~n",
              [tc(fun() -> master_task_manager:add_local_task(
                             ping_pong_node_depend_log,
                             start_link, [],"some text")
                  end, "Add new task")]),
    % tc(fun() -> master_task_manager:add_local_task(
    %               ping_pong_node_depend_log, start_link, [],"some text")
    %    end, "Add new task"),

    ?SLEEP,
    
    tc(fun() -> pong = ping_pong_node_depend_log:ping(?N3)
       end, "Ping pong"),

    ?SSLEEP,
    {ok, 1} = master_node:i_get_table_size(ping_pong_node_depend_log),
    
    tc(fun() -> ok = node_down(?N3)
       end, "Node down"),

    % need for get message about node down (node down call guarantees
    % only that the node does not answer)
    ?SLEEP,

    {ok, 0} = master_node:i_get_table_size(ping_pong_node_depend_log),
    {ok, [_,_,_]} = master_node:i_nodes(),

    tc(fun() -> pong = ping_pong_node_depend_log:ping(?N1),
                {badrpc,_} = call(?N1, ping_pong_node_depend_log,
                                  ping, [?N3]),
                pong = call(?N1, ping_pong_node_depend_log, ping, [?N4]),
                pong = call(?N1, ping_pong_node_depend_log, ping, [])
       end, "Ping pong 3 time correct. 1 time bad"),
    ?SSLEEP,
    {ok, 3} = master_node:i_get_table_size(ping_pong_node_depend_log),

    tc(fun () -> ok = node_up(?N3),
                 ok = upload(?N3),
                 ok = connect(?N3, ?N1)
       end, "New node up and connect"),

    {ok, [_,_,_,_]} = master_node:i_nodes(),
    pong = call(?N3, ping_pong_node_depend_log, ping, []),

    ?SSLEEP,
    {ok, 4} = master_node:i_get_table_size(ping_pong_node_depend_log),
    ok.



%%%===================================================================
%%% Internal functions
%%%===================================================================


connect(Node, MasterNode) ->
    call(Node, slave_node, connect, [MasterNode]).



node_up(Node) ->
    URL = re:replace(atom_to_list(Node),"^[^@]*@","",[{return,list}]),
    open_port({spawn_executable, ?SSH},
              [stream, {args, [URL,
                               "-o", "PreferredAuthentications=publickey",
                               "-f", "erl -detached -noinput -name \""
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

