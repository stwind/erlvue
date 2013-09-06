-module(erlvue_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("logger.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = erlvue_sup:start_link(),
    setup_cowboy(),
    erlvue_pubsub:setup(),
    erlvue_session:setup(),
    {ok, Sup}.

stop(_State) ->
    ok.

%% ===================================================================
%% Private
%% ===================================================================

setup_cowboy() ->
    StartFun = start_fun(),
    NbAcceptors = get_env(acceptors, 100),
    {ok, _} = cowboy:StartFun(erlvue, NbAcceptors, trans_opts(), proto_opts()).

start_fun() ->
    case get_env(ssl, false) of
        true ->
            start_https;
        false ->
            start_http
    end.

get_env(Key, Default) ->
    case application:get_env(erlvue, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

proto_opts() ->
    [
        {env, [{dispatch, dispatch()}]}, 
        {timeout, get_env(timeout, 300000)}
    ].

trans_opts() ->
    {ok, Ip} = inet_parse:address(get_env(ip, "0.0.0.0")),
    Opts = [
        {port, get_env(port, 9081)}, {ip, Ip}, 
        {max_connections, get_env(max_connections, 1024)}, 
        {backlog, get_env(backlog, 1024)}
    ],
    case get_env(ssl, true) of
        true ->
            Opts1 = [
                {certfile,get_env(certfile, "")},
                {keyfile,get_env(keyfile, "")} 
                | Opts
            ],
            case get_env(cacertfile, "") of
                [] ->
                    Opts1;
                CaCertFile ->
                    [{cacertfile,CaCertFile} | Opts1]
            end;
        false ->
            Opts
    end.

dispatch() ->
    DispatchFile = get_env(dispatch_file, "priv/dispatch.script"),
    {ok, Dispatch} = file:script(DispatchFile, bs()),
    SockjsOpts = [{logger, fun(_,R,_) -> R end}], %% TODO: use a logger
    Wamp = wamp:init_sockjs_state(<<"/wamp">>, erlvue_wamp, [], 
        wamp_uri_map(), SockjsOpts),
    WampRoute = {<<"/wamp/[...]">>, sockjs_cowboy_handler, Wamp},
    Routes = erlvue_util:kf('_', Dispatch),
    Dispatch1 = lists:keyreplace('_', 1, Dispatch, {'_', [WampRoute | Routes]}),
    cowboy_router:compile(Dispatch1).

bs() ->
    erl_eval:new_bindings().

wamp_uri_map() ->
    UriFile = get_env(wamp_uri_file, "priv/wamp_uri.script"),
    {ok, UriMap} = file:script(UriFile, bs()),
    UriMap.
