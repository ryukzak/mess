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
        ]).

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

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
