-module(erlvue_cb_ping).

%-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    Req1 = cowboy_req:set_resp_header(<<"server">>, erlvue_util:to_b(node()), Req),
    {ok, Req2} = cowboy_req:reply(200, [], <<"pong">>, Req1),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
