-module(erlvue_topic).

-export([procs/2]).

-include("common.hrl").

%% ===================================================================
%% Public
%% ===================================================================

procs(Node, Type) ->
    NodeBin = ?urlenc(?to_b(Node)),
    type(<<"/procs/", NodeBin/binary>>, Type).

%% ===================================================================
%% Privte
%% ===================================================================

type(Topic, Type) ->
    <<Topic/binary, "?type=", Type/binary>>.
