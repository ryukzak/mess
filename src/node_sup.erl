%%%-------------------------------------------------------------------
%%% @author ryukzak <>
%%% @copyright (C) 2010, ryukzak
%%% @doc
%%%
%%% @end
%%% Created :  3 Mar 2010 by ryukzak <>
%%%-------------------------------------------------------------------
-module(node_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
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
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
init([]) ->
    {ok, {{one_for_one, 5, 10},
          [
           {controller_listener_sup,
            {listener_sup, start_link,
             [controller_listener_sup, controller_listener, 9993, rfid_reader]},
            permanent, 2000, supervisor, [listener_sup]}
           
%% 					 , {teminal_listener_sup,
%% 							{listener_sup, start_link,
%% 							 [terminal_listener_sup, terminal_listener, 9994, terminal]},
%% 							permanent, 2000, supervisor, [listener_sup]}
           
           , {clerk, {clerk, start_link, []},
              permanent, 2000, worker, [clerk_sup]}
           
           , {clerk_sup, {clerk_sup, start_link, [9990]},
              permanent, 2000, supervisor, [clerk_sup]}
           
           , {wizard, {wizard, start_link, []},
              permanent, 2000, worker, [wizard]}
          ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
