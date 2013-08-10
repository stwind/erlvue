-module(erlvue_pubsub).

-export([init/0]).
-export([subscribe/2]).
-export([unsubscribe/2]).
-export([publish/2]).
-export([publish/3]).
-export([publish/4]).

-include("logger.hrl").
-include("common.hrl").

-define(TOPICS, topics).
-define(CONNS, conns).

%% ===================================================================
%% Public
%% ===================================================================

init() ->
    ets:new(?TOPICS, [named_table, public]),
    ets:new(?CONNS, [named_table, public]).

subscribe(Topic, {Id, Conn}) ->
    Topic1 = ?md5(Topic),
    ets:insert(?TOPICS, {Topic1, dict:store(Id, Conn, get_subs(Topic1))}),
    ets:insert(?CONNS, {Id, Conn}),
    ok.

unsubscribe(Topic, {Id, _}) ->
    Topic1 = ?md5(Topic),
    ets:insert(?TOPICS, {Topic1, dict:erase(Id, get_subs(Topic1))}),
    ets:delete(?CONNS, Id),
    ok.

publish(Topic, Event) ->
    publish(Topic, Event, [], []).

publish(Topic, Event, Exclude) ->
    publish(Topic, Event, Exclude, []).

publish(Topic, Event, Exclude, Eligible) ->
    Subs = get_subs(?md5(Topic)),
    Subs1 = dict:to_list(pick_subs(Subs, Exclude, Eligible)),
    ?debug("publishing to ~p",[Subs1]),
    send(Topic, Event, Subs1),
    ok.

%% ===================================================================
%% Private
%% ===================================================================

get_subs(Topic) ->
    case ets:lookup(?TOPICS, Topic) of
        [{Topic, Subs}] -> Subs;
        [] -> dict:new()
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
