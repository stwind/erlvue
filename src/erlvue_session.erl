-module(erlvue_session).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3,
         handle_cast/2]).

-export([start_link/1]).
-export([start_it/1]).
-export([setup/0]).
-export([lookup/1]).

-include("logger.hrl").
-include("common.hrl").

-record(state, {
        id = <<>> :: binary(),
        conn :: term()
    }).

-define(SESSIONS, erlvue_sessions).

%% ===================================================================
%% Public
%% ===================================================================

start_link(Client) ->
    gen_server:start_link(?MODULE, [Client], []).

start_it({Id, _} = Client) ->
    case lookup(Id) of
        {error, notfound} ->
            erlvue_session_sup:start_child(Client);
        {ok, Pid} ->
            {ok, Pid}
    end.

setup() ->
    ets:new(?SESSIONS, [named_table, public]).

lookup(Id) ->
    case ets:lookup(?SESSIONS, Id) of
        [{Id, Pid}] ->
            {ok, Pid};
        [] ->
            {error, notfound}
    end.

%% ===================================================================
%% gen_server
%% ===================================================================

init([{Id, Conn}]) ->
    ?debug("session ~p started", [Id]),
    reg(Id),
    {ok, #state{ id = Id, conn = Conn }}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({pubsub_event, <<"/proc/",_/binary>> = Topic, Event}, #state{conn = Conn} = State) ->
    ?debug("topic ~p",[Topic]),
    wamp:notify(Conn, Topic, Event),
    {noreply, State};

handle_info({pubsub_event, Topic, Event}, #state{conn = Conn} = State) ->
    wamp:notify(Conn, Topic, Event),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Private
%% ===================================================================

reg(Id) ->
    ets:insert(?SESSIONS, {Id, self()}).
