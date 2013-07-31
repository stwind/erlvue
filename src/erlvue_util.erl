-module(erlvue_util).

-export([to_str/1]).
-export([to_a/1]).
-export([to_i/1]).
-export([to_l/1]).
-export([to_b/1]).
-export([to_bool/1]).

-export([kf/2]).
-export([kf/3]).

-type value() :: string() | binary() | integer() | atom().

%% ===================================================================
%% Api functions
%% ===================================================================

%% @doc Convert values to integer
-spec to_i(value()) -> integer().
to_i(Value) when is_list(Value) ->
    list_to_integer(Value);
to_i(Value) when is_integer(Value) ->
    Value;
to_i(Value) ->
    to_i(to_l(Value)).

%% @doc Convert values to binary
-spec to_b(value()) -> binary().
to_b(Value) when is_list(Value) ->
    list_to_binary(Value);
to_b(Value) when is_binary(Value) ->
    Value;
to_b(Value) ->
    to_b(to_l(Value)).

%% @doc Convert values to string
-spec to_l(value()) -> string().
to_l(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_l(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_l(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_l(Value) when is_float(Value) ->
    mochinum:digits(Value);
to_l(Value) ->
    Value.

%% @doc Convert values to atom
-spec to_a(value()) -> atom().
to_a(V) when is_list(V) ->
    list_to_atom(V);
to_a(V) when is_atom(V) ->
    V;
to_a(V) ->
    to_a(to_l(V)).

%% @doc Format value to string
-spec to_str(term()) -> string().
to_str(Term) ->
    y_str:oneline(io_lib:format("~p",[Term])).

%% @doc Convert values to boolean
-spec to_bool(boolean() | integer()) -> boolean().
to_bool(Bool) when is_boolean(Bool) -> Bool;
to_bool(0) -> false;
to_bool(_) -> true.

%% @see kf/3
-spec kf(term(),[{term(),term()}]) -> term().
kf(Key, List) ->
    kf(Key, List, undefined).

%% @doc shorter form of {@link proplists:get_value/3}
-spec kf(term(),[{term(),term()}], term()) -> term().
kf(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {Key, Value} -> Value
    end.

