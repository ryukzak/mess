%%%-------------------------------------------------------------------
%%% @author Ryukzak Neskazov <>
%%% @copyright (C) 2010, Ryukzak Neskazov
%%% @doc
%%%
%%% @end
%%% Created : 25 Apr 2010 by Ryukzak Neskazov <>
%%%-------------------------------------------------------------------
-module(util).

%% API
-export([
         start_mnesia/0
         , next_value/1
        ]).

-include_lib("tables.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_mnesia() ->
    case application:start(mnesia) of
        ok -> ok;
        {error,{already_started,mnesia}} -> ok;
        {error,Reason} ->
            error_logger:error_report(["Can't start application"
                                       , {"Application:", mnesia}
                                       , {"Error reason:", Reason}
                                      ]),
            {error,Reason}
    end.

next_value(Name) ->
    % May be call only in transaction.
    [Counter] = mnesia:read(counter, Name),
    Value = Counter#counter.value,
    mnesia:write(#counter{name = Name
                          , value = Value + 1}),
    Value.
    

    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
