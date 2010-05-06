%%%-------------------------------------------------------------------
%%% @author ryukzak <>
%%% @copyright (C) 2010, ryukzak
%%% @doc
%%%
%%% @end
%%% Created :  3 Mar 2010 by ryukzak <>
%%%-------------------------------------------------------------------
-module(channel, [Module]).

-behaviour(gen_server).

%% API
-export([
         start_link/2
         , parse/3
         , cast/2
         , tables/0
         , clean/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {sock, buffer = [], chanel_state = undefined}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Sock, Pid) ->
    gen_server:start_link({?MODULE, Module}, {Sock, Pid}, []).

cast(Pid, Msg) ->
    gen_server:cast(Pid, {cast, Msg}).

tables() ->
    Module:tables().

clean(DownNode) ->
    Module:clean(DownNode).


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
init({Sock, Pid}) ->
    gate:take_socket(Sock, Pid),
    {ok, #state{sock=Sock}}.

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
handle_cast({cast, Msg}, #state{chanel_state = CS
                                , sock = Sock
                               } = State) ->
    case catch Module:cast(Msg, CS) of
        {msg, Msg, CS1} -> gen_tcp:send(Sock, Msg);
        % {'EXIT', _} -> CS1 = CS;
        CS1 -> ok
    end,
    {noreply, State#state{chanel_state = CS1}};

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

handle_info({tcp, _Port, Msg} = _Info, #state{buffer = Buffer
                                              , chanel_state = CS
                                             } = State) ->
    io:format("trace~n"),
    {Buffer1, CS1} = tcp_loop(Buffer ++ Msg, CS),
    {noreply, State#state{buffer = Buffer1
                          , chanel_state = CS1
                         }};

handle_info({tcp_closed, _Port} = _Info, State) ->
    {stop, normal, State};

handle_info(Info, State) ->
    error_logger:warning_msg("Unknown message in chanel: ~p", [Info]),
    {noreply, State}.

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
terminate(_Reason, #state{sock = Sock
                          , chanel_state = CS
                         } = _State) ->
    catch Module:terminate(CS),
    gen_tcp:close(Sock).

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

parse(_Acc, _Wacc, []) ->
    none;

parse(Acc, [], [10|Buffer]) ->
    {lists:reverse(Acc), [], Buffer};

parse(Acc, Wacc, [10|Buffer]) ->
    {lists:reverse([lists:reverse(Wacc) | Acc]), [], Buffer};


parse(Acc, Wacc, [13|Buffer]) -> % FIXME need to delete
    parse(Acc, Wacc, Buffer);

parse(Acc, [], [81|Buffer]) ->
    {lists:reverse(Acc), [], Buffer};
parse(Acc, Wacc, [81|Buffer]) ->
    {lists:reverse([lists:reverse(Wacc) | Acc]), [], Buffer};

parse(Acc, [], [32|Buffer]) ->
    parse(Acc, [], Buffer);

parse(Acc, Wacc, [32|Buffer]) ->
    parse([lists:reverse(Wacc) | Acc], [], Buffer);

parse(Acc, Wacc, [X|Buffer]) ->
    parse(Acc, [X | Wacc], Buffer).



tcp_loop(Buffer, State) ->
    case parse([], [], Buffer) of
        none -> {Buffer, State};
        {Msg, [], Buffer1} -> State1 = Module:tcp_cast(Msg, State),
                              tcp_loop(Buffer1, State1);
        X -> error_logger:error_msg("Error in chanel parser: ~p", [X])
    end.
