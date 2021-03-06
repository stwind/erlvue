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

handle_wamp_call({nodes, _Uri, [<<"read">>]}, _Client, State) ->
    {{ok, nodes_resp()}, State};

handle_wamp_call({procs, Uri, [<<"read">>]}, _Client, State) ->
    Node = ?to_a(wamp_uri:binding(node, Uri)),
    Server = maybe_start_procs_worker(Node),
    {erlvue_procs:fetch(Server), State};

handle_wamp_call({stats, Uri, [<<"read">>]}, _Client, State) ->
    Node = ?to_a(wamp_uri:binding(node, Uri)),
    Server = maybe_start_stats_worker(Node),
    {erlvue_stats:fetch(Server), State};

handle_wamp_call({proc, Uri, [<<"read">>]}, _Client, State) ->
    Node = ?to_a(wamp_uri:binding(node, Uri)),
    TPid = list_to_pid(?to_l(?urldec(wamp_uri:binding(pid, Uri)))),
    Server = maybe_start_proc_worker(Node, TPid),
    {erlvue_stats:fetch(Server), State};

handle_wamp_call(_, _, State) ->
    {{ok, <<>>}, State}.

handle_wamp_sub({procs, Uri}, _, #state{session = Pid} = State) ->
    Node = ?to_a(wamp_uri:binding(node, Uri)),
    Type = wamp_uri:qs_val(<<"type">>, Uri),
    Topic = erlvue_topic:procs(Node, Type),
    maybe_start_procs_worker(Node),
    ok = erlvue_pubsub:subscribe(Topic, Pid),
    {ok, State};

handle_wamp_sub({proc, Uri}, _, #state{session = Pid} = State) ->
    Node = ?to_a(wamp_uri:binding(node, Uri)),
    TPid = list_to_pid(?to_l(?urldec(wamp_uri:binding(pid, Uri)))),
    Topic = erlvue_topic:proc(Node, TPid),
    maybe_start_proc_worker(Node, TPid),
    ok = erlvue_pubsub:subscribe(Topic, Pid),
    {ok, State};

handle_wamp_sub({stats, Uri}, _, #state{session = Pid} = State) ->
    Node = ?to_a(wamp_uri:binding(node, Uri)),
    Topic = erlvue_topic:stats(Node),
    maybe_start_stats_worker(Node),
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

terminate(_, #state{session = Pid} = State) ->
    ok = erlvue_pubsub:unsubscribe(Pid),
    {ok, State}.

%% ===================================================================
%% Private
%% ===================================================================

nodes_resp() ->
    [ej:set({"name"}, {[]}, N) || N <- [node() | nodes()]].

maybe_start_procs_worker(Node) ->
    maybe_started(erlvue_procs:new(Node)).

maybe_start_proc_worker(Node, Pid) ->
    maybe_started(erlvue_proc:new(Node, Pid)).

maybe_start_stats_worker(Node) ->
    maybe_started(erlvue_stats:new(Node)).

maybe_started({ok, Pid}) ->
    Pid;
maybe_started({error, {already_started, Pid}}) ->
    Pid.
