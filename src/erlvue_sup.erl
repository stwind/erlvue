-module(erlvue_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = [worker_sup(),session_sup()],
    {ok, { {one_for_one, 5, 10}, Children} }.

worker_sup() ->
    ?CHILD(erlvue_worker_sup, supervisor).

session_sup() ->
    ?CHILD(erlvue_session_sup, supervisor).
