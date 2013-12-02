-module(erlvue_procs).

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

-record(state, { 
        node = node() :: node(),
        infos = []  %% TODO: add json object type spec
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

fetch(Srv) ->
    gen_server:call(Srv, fetch).

%% ===================================================================
%% gen_server
%% ===================================================================

init([Node]) ->
    timer:send_interval(?INTERVAL, refresh),
    self() ! refresh,
    {ok, #state{ node = Node }}.

handle_call(fetch, _From, #state{infos = Infos} = State) ->
    {reply, {ok, Infos}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({add, P}, State) ->
    {noreply, add_proc(P, State)};

handle_cast({remove, P}, State) ->
    {noreply, remove_proc(P, State)};

handle_cast(refresh, State) ->
    {noreply, notify(<<"reset">>, collect_all(State))};

handle_cast(clear, State) ->
    {noreply, clear_procs(State)};

handle_cast(dummy, State) ->
    {noreply, dummy(State)};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh, State) ->
    {noreply, notify(<<"reset">>, collect_all(State))};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Private
%% ===================================================================

collect_all(State) ->
    lists:foldl(fun add_proc/2, State#state{infos = []}, 
        erlvue_util:take(20, processes())).

collect_info(P, Node) ->
    case process_info(P, fields()) of
        undefined ->
            undefined;
        [{registered_name,Reg},{initial_call,Initial},{memory,Mem},
            {reductions,Reds},{current_function,Current},
            {message_queue_len,Qlen}] ->
            Name = case Reg of
                [] -> fmt_mfa(init_call(Initial, P));
                _ -> Reg
            end,
            erlvue_util:to_obj([
                    {name, Name},{mem, Mem},{mq, Qlen},{pid, P},
                    {reds, Reds},{cf, fmt_mfa(Current)}, {node, ?to_b(Node)}
                ])
    end.

fields() ->
    [registered_name, initial_call, memory,reductions, 
     current_function, message_queue_len].

init_call({proc_lib, init_p, _}, Pid) ->
    proc_lib:translate_initial_call(Pid);
init_call(Initial, _Pid) ->
    Initial.

add_proc(P, #state{infos = Infos, node = Node} = State) ->
    Infos1 = case collect_info(P, Node) of
        undefined -> 
            Infos;
        Info -> 
            erlvue_pubsub:publish(erlvue_topic:procs(Node, <<"add">>), Info),
            [Info | Infos]
    end,
    State#state{infos = Infos1}.

remove_proc(P, #state{infos = Infos, node = Node} = State) ->
    Infos1 = case [I || I <- Infos, get_pid(I) == P] of
        [] ->
            Infos;
        ToRemove ->
            lists:foreach(fun(I) -> 
                        erlvue_pubsub:publish(erlvue_topic:procs(Node, <<"remove">>), I)
                end, ToRemove),
            Infos -- ToRemove
    end,
    State#state{infos = Infos1}.

dummy(State) ->
    notify(<<"reset">>, State#state{infos = [
                erlvue_util:to_obj([
                        {pid, list_to_pid("<0.101.0>")},
                        {name, p1},{mem, 1234},{mq, 0},
                        {reds, 190022}, {cf, none}
                    ]),
                erlvue_util:to_obj([
                        {pid, list_to_pid("<0.102.0>")},
                        {name, p4},{mem, 123214},{mq, 0},
                        {reds, 234823}, {cf, none}
                    ])
            ]}).

clear_procs(State) ->
    notify(<<"reset">>, State#state{infos = []}).

notify(Type, #state{node = Node, infos = Infos} = State) ->
    erlvue_pubsub:publish(erlvue_topic:procs(Node, Type), Infos),
    State.

child_spec(Node) ->
    {id(Node), {?MODULE, start_link, [Node]}, permanent, 5000, worker, [?MODULE]}.

id(Node) ->
    ?to_a(?to_l(?MODULE) ++ "_" ++ ?to_l(Node)).

get_pid({Info}) ->
    list_to_pid(?to_l(?kf(pid, Info))).

fmt_mfa(MFA) ->
    erlvue_util:fmt_mfa(MFA).
