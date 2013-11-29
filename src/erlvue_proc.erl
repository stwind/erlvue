-module(erlvue_proc).

-behaviour(gen_server).

-export([start_link/2]).
-export([new/2]).
-export([stop/1]).
-export([collect_info/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("logger.hrl").
-include("common.hrl").

-define(SERVER, ?MODULE).
-define(INTERVAL, 5000).

-record(state, { 
        node = node() :: node(),
        pid :: pid(),
        info = [] :: list({atom(), term()})
    }).

%% ===================================================================
%% Pbulic
%% ===================================================================

start_link(Node, Pid) ->
    gen_server:start_link(?MODULE, [Node, Pid], []).

new(Node, Pid) ->
    erlvue_worker_sup:start_child(child_spec(Node, Pid)).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% ===================================================================
%% gen_server
%% ===================================================================

init([Node, Pid]) ->
    timer:send_interval(?INTERVAL, refresh),
    {ok, #state{ node = Node, pid = Pid }}.

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

child_spec(Node, Pid) ->
    PidStr = pid_str(Pid),
    {?to_a(?to_l(?MODULE) ++ "_" ++ ?to_l(Node) ++ "_" ++ PidStr), 
     {?MODULE, start_link, [Node, Pid]}, permanent, 5000, worker, [?MODULE]}.

pid_str(Pid) ->
    [[$< | A], B, C0] = string:tokens(pid_to_list(Pid), "."),
    [$> | C1] = lists:reverse(C0),
    A ++ "_" ++ B ++ "_" ++ lists:reverse(C1).

notify(#state{node = Node, pid = Pid, info = Info} = State) ->
    Topic = erlvue_topic:proc(Node, Pid),
    erlvue_pubsub:publish(Topic, Info),
    State.

collect(#state{node = Node, pid = Pid} = State) ->
    State#state{info = collect_info(Node, Pid)}.

collect_info(Node, Pid) ->
    [
     {registered_name, Reg},
     {initial_call, Initial},
     {memory, Mem},
     {reductions, Reds},
     {current_function, Current},
     {message_queue_len, Qlen},
     {status, Status},
     {group_leader, Leader}
    ] = process_info(Pid, fields()),
    %% TODO: uniform these name with ones in erlvue_procs
    erlvue_util:to_obj(
      [
       {name, Reg}, {mem, Mem}, {mq, Qlen}, {pid, Pid},
       {initial_call, fmt_mfa(Initial)},
       {reds, Reds}, {cf, fmt_mfa(Current)}, {node, ?to_b(Node)},
       {status, Status}, {group_leader, Leader}
      ]).

fmt_mfa(MFA) ->
    erlvue_util:fmt_mfa(MFA).

fields() ->
    [registered_name, initial_call, memory,reductions, 
     current_function, message_queue_len, status, group_leader].
