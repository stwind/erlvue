-module(erlvue_wamp).

-export([setup/1]).
-export([welcome/2]).
-export([handle_wamp_prefix/3]).
-export([handle_wamp_call/3]).
-export([handle_wamp_sub/3]).
-export([handle_wamp_unsub/3]).
-export([handle_wamp_pub/3]).
-export([terminate/2]).

-include("logger.hrl").
-include("common.hrl").

-record(state, {
        session :: pid()
    }).

%% ===================================================================
%% Callbacks
%% ===================================================================

setup([]) ->
    #state{}.

welcome(Client, State) ->
    {ok, Pid} = erlvue_session:start_it(Client),
    {ok, State#state{session = Pid}}.

handle_wamp_prefix({_Prefix, _Uri}, _Client, State) ->
    {ok, State}.

handle_wamp_call({nodes, _Env, [<<"read">>]}, _Client, State) ->
    {{ok, nodes_resp()}, State};

handle_wamp_call(_, _, State) ->
    {{ok, <<>>}, State}.

handle_wamp_sub({procs, Uri}, _, #state{session = Pid} = State) ->
    Node = ?to_a(wamp_uri:binding(node, Uri)),
    Type = wamp_uri:qs_val(<<"type">>, Uri),
    Topic = erlvue_topic:procs(Node, Type),
    maybe_start_worker(Node),
    ok = erlvue_pubsub:subscribe(Topic, Pid),
    {ok, State};

handle_wamp_sub(_, _, State) ->
    {ok, State}.

handle_wamp_unsub({default, Topic}, Client, State) ->
    ok = erlvue_pubsub:unsubscribe(Client, Topic),
    {ok, State}.

handle_wamp_pub({default, Topic, Event, Opts}, _, 
    #state{session = Pid} = State) ->
    Exclude = case ?kf(exclude, Opts) of
        true -> [Pid];
        false -> []
    end,
    erlvue_pubsub:publish(Topic, Event, Exclude, ?kf(eligible, Opts)),
    {ok, State}.

terminate(Client, State) ->
    ok = erlvue_pubsub:unsubscribe(Client),
    {ok, State}.

%% ===================================================================
%% Private
%% ===================================================================

nodes_resp() ->
    [ej:set({"name"}, {[]}, N) || N <- [node() | nodes()]].

maybe_start_worker(Node) ->
    erlvue_procs:start_it(Node).
