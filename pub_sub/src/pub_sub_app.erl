%%%-------------------------------------------------------------------
%% @doc pub_sub public API
%% @end
%%%-------------------------------------------------------------------

-module(pub_sub_app).

-behaviour(application).

-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================
% This is the start of the application, We are starting the supervison tree here
start(_StartType, _StartArgs) ->
    pub_sub_sup:start_link().

% stop(_State) is to stop the application
stop(_State) ->
    ok.
