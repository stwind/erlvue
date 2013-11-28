-module(erlvue_topic).

-export([procs/2]).
-export([proc/2]).
-export([stats/1]).

-include("common.hrl").

%% ===================================================================
%% Public
%% ===================================================================

procs(Node, Type) ->
    NodeBin = ?urlenc(?to_b(Node)),
    type(<<"/procs/", NodeBin/binary>>, Type).

proc(Node, Pid) ->
    NodeBin = ?urlenc(?to_b(Node)),
    PidBin = ?urlenc(?to_b(Pid)),
    type(<<"/proc/", NodeBin/binary, "/", PidBin/binary>>, <<"update">>).

stats(Node) ->
    NodeBin = ?urlenc(?to_b(Node)),
    type(<<"/stats/", NodeBin/binary>>, <<"update">>).

%% ===================================================================
%% Privte
%% ===================================================================

type(Topic, Type) ->
    <<Topic/binary, "?type=", Type/binary>>.
