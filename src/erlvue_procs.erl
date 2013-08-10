-module(erlvue_procs).

-behaviour(gen_server).

-export([start_link/1]).
-export([start_it/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("logger.hrl").
-include("common.hrl").

-define(SERVER, ?MODULE).
-define(INTERVAL, 5000).

-record(state, { node }).

%% ===================================================================
%% Pbulic
%% ===================================================================

start_link(Node) ->
    gen_server:start_link(?MODULE, [Node], []).

start_it(Node) ->
    erlvue_worker_sup:start_child(child_spec(Node)).

%% ===================================================================
%% gen_server
%% ===================================================================

init([Node]) ->
    timer:send_interval(?INTERVAL, update),
    {ok, #state{ node = Node }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update, #state{node = Node} = State) ->
    Infos = collect(processes(), []),
    erlvue_pubsub:publish(erlvue_topic:procs(Node), Infos),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Private
%% ===================================================================

collect([P | Ps], Acc) when P =:= self() ->
    collect(Ps, Acc);
collect([P | Ps], Acc) ->
    Fs = [registered_name, initial_call, memory,reductions,
        current_function, message_queue_len],
    case process_info(P, Fs) of
        undefined ->
            collect(Ps, Acc);
        %% XXX: cleanup this
        [{registered_name,Reg},{initial_call,Initial},{memory,Mem},
            {reductions,Reds},{current_function,Current},
            {message_queue_len,Qlen}] ->
            Name = case Reg of
                [] -> initial_call(Initial, P);
                _ -> Reg
            end,
            Info = erlvue_util:to_obj([
                    {name, Name},{mem, Mem},{mq, Qlen},{pid, P},
                    {reds, Reds}, {cf, Current}
                ]),
            collect(Ps, [Info | Acc])
    end;
collect([], Acc) -> 
    Acc.

initial_call({proc_lib, init_p, _}, Pid) ->
    proc_lib:translate_initial_call(Pid);
initial_call(Initial, _Pid) ->
    Initial.

child_spec(Node) ->
    {?to_a(?to_l(?MODULE) ++ "_" ++ ?to_l(Node)), {?MODULE, start_link, [Node]}, 
        permanent, 5000, worker, [?MODULE]}.
