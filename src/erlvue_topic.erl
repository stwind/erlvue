-module(erlvue_topic).

-export([procs/1]).

-include("common.hrl").

%% ===================================================================
%% Public
%% ===================================================================

procs(Node) ->
    NodeBin = ?urlenc(?to_b(Node)),
    <<"/procs/", NodeBin/binary>>.
