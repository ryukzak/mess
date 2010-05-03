%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(slave_task_manager).

-behaviour(gen_server).

%% API
-export([
         start_link/0
         , add_local_task/2
         , start_all_local_task/0
         , reset_task/1
         , add_atom_task/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include_lib("tables.hrl").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_atom_task(#atom_task{run_on_node = Node} = Task) ->
    gen_server:call({?SERVER, Node}, {add_atom_task, Task}).

add_local_task(Node, Task) ->
    gen_server:call({?SERVER, Node},
                    {add_local_task, Task}).

start_all_local_task() ->
    gen_server:call(?SERVER, start_all_local_task).

reset_task(Node) -> gen_server:call({?SERVER, Node}, reset_task).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ets:new(slave_atom_task, [public, named_table, {heir,none}]),

    process_flag(trap_exit, true),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_atom_task, #atom_task{mfa={M,F,A}
                                      } = Task}, _From, State) ->
    io:format("slave_task add_atom_task~n"),
    Pid = spawn_link(M,F,A),
    ets:insert(slave_atom_task, {Pid, Task}),
    Reply = ok,
    {reply, Reply, State};

handle_call({add_local_task, #local_task{mfa={M,F,A}
                                        }}, _From, State) ->
    % async start task proccess.
    SupResult = local_task_sup:start_child(M, F, A),
    Reply = {ok, M, F, A, SupResult},
    {reply, Reply, State};

handle_call(start_all_local_task, _From, State) ->
    {ok, MFAs} = master_task_manager:get_local_task(),
    [local_task_sup:start_child(MFA) || MFA <- MFAs],
    Reply = ok,
    {reply, Reply, State};

handle_call(reset_task, _From, State) ->
    local_task_sup:terminate_all_child(),
    ets:delete_all_objects(slave_atom_task),
    Reply = ok,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', From, normal} = Msg, State) ->
    io:format("slave_task EXIT normal~n"),
    Task = get_task(From),
    resend_exit_msg(Msg, Task),
    case Task of
        #atom_task{restart = {permanent, _, _}} ->
            restart_or_stop_atom_task(Task);
        _ -> master_task_manager:stop_atom_task(Task)
    end,
    {noreply, State};

handle_info({'EXIT', From, _} = Msg, State) ->
    io:format("slave_task EXIT abnormal~n"),
    Task = get_task(From),
    resend_exit_msg(Msg, Task),
    case Task of
        #atom_task{restart = temporary} ->
            master_task_manager:stop_atom_task(Task);
        #atom_task{restart = {_,_,_}} ->
            restart_or_stop_atom_task(Task)
    end,
    {noreply, State};

handle_info(Info, State) ->
    io:format("Slave task manafer info message: ~p~n",[Info]),
    {noreply, State}.

restart_or_stop_atom_task(#atom_task{restart = {_, MaxR, MaxT}
                                     , history = History
                                    } = Task) ->
    TimeLine = get_time_line(MaxT),
    History1 = [H || H <- History, TimeLine < H],
    io:format(" ~p; History before and after:~n~p~n~p~n",
              [TimeLine,History,History1]),
    if length(History1) >= MaxR ->
            master_task_manager:stop_atom_task(
              Task#atom_task{history = History1});
       true ->
            master_task_manager:restart_atom_task(
              Task#atom_task{history = History1})
    end.

get_time_line(MaxT) ->
    {T,M,S} = now(),
    round(timer:hms(T,M,S)/1000) - MaxT.

get_task(From) ->
    [{_, Task}] = ets:lookup(slave_atom_task, From),
    ets:delete(slave_atom_task, From),
    Task.

resend_exit_msg(Msg, Task) ->
    case Task of
        #atom_task{link=true
                   , from = Pid} -> Pid ! Msg;
        _ -> ok
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
