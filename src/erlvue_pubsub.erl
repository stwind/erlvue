-module(erlvue_pubsub).

-export([init/0]).
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

init() ->
    ets:new(?BY_TOPIC, [named_table, public]),
    ets:new(?BY_ID, [named_table, public]).

subscribe(Topic, {Id, Conn}) ->
    ?debug("~p sub: ~p", [Id, Topic]),
    Topic1 = ?md5(Topic),
    ets:insert(?BY_TOPIC, {Topic1, dict:store(Id, Conn, 
                lookup(?BY_TOPIC, Topic1, dict:new()))}),
    ets:insert(?BY_ID, {Id, [Topic | lookup(?BY_ID, Id, [])]}),
    ok.

unsubscribe({Id, _}, Topic) ->
    ?debug("~p unsub: ~p", [Id, Topic]),
    Topic1 = ?md5(Topic),
    case ets:lookup(?BY_TOPIC, Topic1) of
        [{Topic1, Subs}] ->
            Subs1 = dict:erase(Id, Subs),
            case dict:size(Subs1) of
                0 -> ets:delete(?BY_TOPIC, Topic1);
                _ -> ets:insert(?BY_TOPIC, {Topic1, Subs1})
            end;
        [] ->
            nop
    end,
    case ets:lookup(?BY_ID, Id) of
        [{Id, [Topic]}] ->
            ets:delete(?BY_ID, Id);
        [{Id, Topics}] ->
            ets:insert(?BY_ID, {Id, lists:delete(Topic, Topics)});
        [] ->
            nop
    end,
    ok.

unsubscribe({Id, _} = Client) ->
    [unsubscribe(Client, T) || T <- lookup(?BY_ID, Id, [])],
    ok.

publish(Topic, Event) ->
    publish(Topic, Event, [], []).

publish(Topic, Event, Exclude) ->
    publish(Topic, Event, Exclude, []).

publish(Topic, Event, Exclude, Eligible) ->
    Subs = lookup(?BY_TOPIC, ?md5(Topic), dict:new()),
    Subs1 = dict:to_list(pick_subs(Subs, Exclude, Eligible)),
    send(Topic, Event, Subs1),
    ok.

subscribers(Topic) ->
    Subs = lookup(?BY_TOPIC, ?md5(Topic), dict:new()),
    dict:to_list(Subs).

%% ===================================================================
%% Private
%% ===================================================================

lookup(Tab, Id, Default) ->
    case ets:lookup(Tab, Id) of
        [{Id, Val}] ->  Val;
        [] -> Default
    end.

send(Topic, Event, Subs) ->
    [wamp:notify(C, Topic, Event) || {_, C} <- Subs].

pick_subs(Subs, Exclude, Eligible) ->
    dict:filter(fun(Id, _) -> 
                not lists:member(Id, Exclude) andalso
                case Eligible of
                    [] -> true;
                    _ -> lists:member(Id, Eligible)
                end
        end, Subs).
