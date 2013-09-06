-module(erlvue_session_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(SessionId) ->
    supervisor:start_child(?MODULE, [SessionId]).

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{erlvue_session, {erlvue_session, start_link, []},
            temporary, 5000, worker, [erlvue_session]}]}}.
