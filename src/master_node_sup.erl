%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(master_node_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(FromNode) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [FromNode]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([FromNode]) ->
    {ok, {{one_for_one, 5, 10},
          [{master_node,
            {master_node, start_link, [FromNode]},
            permanent, 2000, worker, [master_node]}
           
           , {master_task_manager,
              {master_task_manager, start_link, []},
              permanent, 2000, worker, [master_task_manager]}
           
           % , {cluster_pool,
           %    {pool, start, [cluster__pool]},
           %    permanent, 2000, worker, [pool]}
           
           % , {global_task_sup, {global_task_sup, start_link, []},
           %    permanent, 2000, supervisor, [global_task_sup]}
           
          ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
