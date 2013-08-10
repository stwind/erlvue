-module(erlvue_wamp).

-export([init/1]).
-export([welcome/2]).
-export([prefix/3]).
-export([call/3]).
-export([subscribe/3]).
-export([unsubscribe/3]).
-export([publish/3]).

-record(state, { }).

-include("logger.hrl").
-include("common.hrl").

%% ===================================================================
%% Callbacks
%% ===================================================================

init([]) ->
    #state{}.

welcome(_, State) ->
    {ok, State}.

prefix(_, _, State) ->
    {ok, State}.

call(_, {<<"echo">>, [Msg]}, State) ->
    {{ok, Msg}, State};

call(_, {<<"/">>, [<<"read">>]}, State) ->
    {{ok, ej:set({"name"},{[]},<<"shuang">>)}, State};

call(_, {<<"/nodes">>, [<<"read">>]}, State) ->
    {{ok, nodes_resp()}, State};

call(_, _, State) ->
    {{ok, <<>>}, State}.

subscribe(Client, <<"/procs/", Node/binary>> = Topic, State) ->
    maybe_start_worker(?to_a(?urldec(Node))),
    ok = erlvue_pubsub:subscribe(Topic, Client),
    {ok, State};

subscribe(_, _, State) ->
    {ok, State}.

unsubscribe(Client, Topic, State) ->
    ok = erlvue_pubsub:unsubscribe(Topic, Client),
    {ok, State}.

publish({Id, _}, {Topic, Event, true, Eligible}, State) ->
    erlvue_pubsub:publish(Topic, Event, [Id], Eligible),
    {ok, State};

publish(_, {Topic, Event, false, Eligible}, State) ->
    erlvue_pubsub:publish(Topic, Event, [], Eligible),
    {ok, State}.

%% ===================================================================
%% Private
%% ===================================================================

nodes_resp() ->
    [ej:set({"name"}, {[]}, N) || N <- [node() | nodes()]].

maybe_start_worker(Node) ->
    _ = erlvue_procs:start_it(Node).
