-module(erlvue_procs).

-behaviour(gen_server).

-export([start_link/1]).
-export([start_it/1]).
-export([stop/1]).

-export([add/1]).
-export([remove/1]).
-export([refresh/0]).
-export([clear/0]).
-export([procs/0]).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Node], []).

start_it(Node) ->
    erlvue_worker_sup:start_child(child_spec(Node)).

stop(Pid) ->
    gen_server:cast(Pid, stop).

add(P) ->
    gen_server:cast(?MODULE, {add, P}).

remove(P) ->
    gen_server:cast(?MODULE, {remove, P}).

refresh() ->
    gen_server:cast(?MODULE, refresh).

clear() ->
    gen_server:cast(?MODULE, clear).

procs() ->
    gen_server:call(?MODULE, procs).

%% ===================================================================
%% gen_server
%% ===================================================================

init([Node]) ->
    timer:send_interval(?INTERVAL, refresh),
    {ok, #state{ node = Node }}.

handle_call(procs, _From, #state{infos = Infos} = State) ->
    {reply, [?kf(pid,I) || {I} <- Infos], State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({add, P}, State) ->
    {noreply, notify(add_proc(P, State))};

handle_cast({remove, P}, State) ->
    {noreply, notify(remove_proc(P, State))};

handle_cast(refresh, State) ->
    {noreply, notify(refresh_procs(State))};

handle_cast(clear, State) ->
    {noreply, notify(clean_procs(State))};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh, State) ->
    {noreply, notify(collect_all(State))};

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

collect_info(P) ->
    case process_info(P, fields()) of
        undefined ->
            undefined;
        [{registered_name,Reg},{initial_call,Initial},{memory,Mem},
            {reductions,Reds},{current_function,Current},
            {message_queue_len,Qlen}] ->
            Name = case Reg of
                [] -> init_call(Initial, P);
                _ -> Reg
            end,
            erlvue_util:to_obj([
                    {name, Name},{mem, Mem},{mq, Qlen},{pid, P},
                    {reds, Reds},{cf, Current}
                ])
    end.

fields() ->
    [registered_name, initial_call, memory,reductions, 
        current_function, message_queue_len].

init_call({proc_lib, init_p, _}, Pid) ->
    proc_lib:translate_initial_call(Pid);
init_call(Initial, _Pid) ->
    Initial.

add_proc(P, #state{infos = Infos} = State) ->
    Infos1 = case collect_info(P) of
        undefined -> Infos;
        Info -> [Info | Infos]
    end,
    State#state{infos = Infos1}.

remove_proc(P, #state{infos = Infos} = State) ->
    State#state{infos = [I || I <- Infos, get_pid(I) /= P]}.

refresh_procs(#state{infos = Infos} = State) ->
    State#state{infos = [collect_info(get_pid(I)) || I <- Infos]}.

clean_procs(State) ->
    State#state{infos = []}.

notify(#state{node = Node, infos = Infos} = State) ->
    erlvue_pubsub:publish(erlvue_topic:procs(Node), Infos),
    State.

child_spec(Node) ->
    {?to_a(?to_l(?MODULE) ++ "_" ++ ?to_l(Node)), {?MODULE, start_link, [Node]}, 
        permanent, 5000, worker, [?MODULE]}.

get_pid({Info}) ->
    list_to_pid(?to_l(?kf(pid, Info))).
