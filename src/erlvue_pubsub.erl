-module(erlvue_pubsub).

-export([setup/0]).
-export([subscribe/2]).
-export([unsubscribe/1]).
-export([unsubscribe/2]).
-export([publish/2]).
-export([publish/3]).
-export([publish/4]).

-export([subscribers/1]).

-include("logger.hrl").
-include("common.hrl").

-define(BY_TOPIC, topics).
-define(BY_ID, conns).

%% ===================================================================
%% Public
%% ===================================================================

setup() ->
    ets:new(?BY_TOPIC, [named_table, public]),
    ets:new(?BY_ID, [named_table, public]).

subscribe(Topic, Pid) ->
    ?debug("~p sub: ~p", [Pid, Topic]),
    Topic1 = ?md5(Topic),
    ets:insert(?BY_TOPIC, {Topic1, 
            lists:usort([Pid | lookup(?BY_TOPIC, Topic1, [])])}),
    ets:insert(?BY_ID, {Pid, [Topic | lookup(?BY_ID, Pid, [])]}),
    ok.

unsubscribe(Pid, Topic) ->
    ?debug("~p unsub: ~p", [Pid, Topic]),
    Topic1 = ?md5(Topic),
    case ets:lookup(?BY_TOPIC, Topic1) of
        [{Topic1, Subs}] ->
            case lists:delete(Pid, Subs) of
                [] -> ets:delete(?BY_TOPIC, Topic1);
                Subs1 -> ets:insert(?BY_TOPIC, {Topic1, Subs1})
            end;
        [] ->
            nop
    end,
    case ets:lookup(?BY_ID, Pid) of
        [{Pid, [Topic]}] ->
            ets:delete(?BY_ID, Pid);
        [{Pid, Topics}] ->
            ets:insert(?BY_ID, {Pid, lists:delete(Topic, Topics)});
        [] ->
            nop
    end,
    ok.

unsubscribe(Pid) ->
    [unsubscribe(Pid, T) || T <- lookup(?BY_ID, Pid, [])],
    ok.

publish(Topic, Event) ->
    publish(Topic, Event, [], []).

publish(Topic, Event, Exclude) ->
    publish(Topic, Event, Exclude, []).

publish(Topic, Event, Exclude, Eligible) ->
    Subs = lookup(?BY_TOPIC, ?md5(Topic), []),
    Subs1 = pick_subs(Subs, Exclude) ++ Eligible,
    send(Topic, Event, Subs1),
    ok.

subscribers(Topic) ->
    lookup(?BY_TOPIC, ?md5(Topic), []).

%% ===================================================================
%% Private
%% ===================================================================

lookup(Tab, Id, Default) ->
    case ets:lookup(Tab, Id) of
        [{Id, Val}] ->  Val;
        [] -> Default
    end.

send(Topic, Event, Subs) ->
    [Pid ! {pubsub_event, Topic, Event} || Pid <- Subs].

pick_subs(Subs, Exclude) ->
    [Pid || Pid <- Subs, not lists:member(Pid, Exclude)].
