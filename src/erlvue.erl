-module(erlvue).

-export([start/0]).

%% ===================================================================
%% Public
%% ===================================================================

start() ->
    application:start(?MODULE).
