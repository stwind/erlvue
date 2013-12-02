-module(erlvue_stats).

-behaviour(gen_server).

-export([start_link/1]).
-export([new/1]).
-export([stop/1]).
-export([fetch/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("logger.hrl").
-include("common.hrl").

-define(SERVER, ?MODULE).
-define(INTERVAL, 5000).

-type key() :: atom().
-type val() :: binary() | integer().

-record(state, { 
        node = node() :: node(),
        stats = [] :: list({key(), val()})
    }).

%% ===================================================================
%% Pbulic
%% ===================================================================

start_link(Node) ->
    gen_server:start_link(?MODULE, [Node], []).

new(Node) ->
    erlvue_worker_sup:start_child(child_spec(Node)).

stop(Pid) ->
    gen_server:cast(Pid, stop).

fetch(Server) ->
    gen_server:call(Server, fetch).

%% ===================================================================
%% gen_server
%% ===================================================================

init([Node]) ->
    timer:send_interval(?INTERVAL, refresh),
    self() ! refresh,
    {ok, #state{ node = Node }}.

handle_call(fetch, _From, #state{stats = Stats} = State) ->
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh, State) ->
    {noreply, notify(collect(State))};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Private
%% ===================================================================

child_spec(Node) ->
    {?to_a(?to_l(?MODULE) ++ "_" ++ ?to_l(Node)), {?MODULE, start_link, [Node]}, 
        permanent, 5000, worker, [?MODULE]}.

notify(#state{node = Node, stats = Stats} = State) ->
    erlvue_pubsub:publish(erlvue_topic:stats(Node), Stats),
    State.

collect(#state{node = Node} = State) ->
    State#state{stats = do_collect(Node)}.

do_collect(Node) ->
    {{input,In},{output,Out}} = erlang:statistics(io),
    {_, Reds} = erlang:statistics(reductions),
    erlvue_util:to_obj(
      [
       {node, ?to_b(Node)},
       {proc_count, erlang:system_info(process_count)},
       {run_queue, erlang:statistics(run_queue)},
       {memory, erlang:memory()},
       {io_in, In}, {io_out, Out},
       {reductions, Reds}
      ]).
