%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(local_task_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0
         , start_child/3
         , start_child/1
         , terminate_all_child/0
         , info/0
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

start_child(MFA) ->
    {M, _, _} = MFA,
    supervisor:start_child(?SERVER,
                           {MFA, MFA, permanent, 2000, worker, [M]}).

start_child(M, F, A) ->
    start_child({M, F, A}).

terminate_all_child() ->
    ChildList = supervisor:which_children(?SERVER),
    [begin
         supervisor:terminate_child(?SERVER, Id),
         supervisor:delete_child(?SERVER, Id)
     end || {Id,_,_,_} <- ChildList].

info() ->
    supervisor:which_children(?SERVER).
    

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
    {ok, {{one_for_one, 5, 10}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
