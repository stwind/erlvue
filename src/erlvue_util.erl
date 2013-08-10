-module(erlvue_util).

-export([to_str/1]).
-export([to_a/1]).
-export([to_i/1]).
-export([to_l/1]).
-export([to_b/1]).
-export([to_bool/1]).

-export([kf/2]).
-export([kf/3]).

-export([md5_hex/1]).

-export([to_obj/1]).

-export([urldecode/1]).
-export([urlencode/1]).

-define(IS_PRIMITIVE(V), 
    is_binary(V);
    is_float(V);
    is_integer(V);
    is_boolean(V);
    V == null).

-type value() :: string() | binary() | integer() | atom().

%% ===================================================================
%% Public
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
    oneline(io_lib:format("~p",[Term])).

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

md5_hex(S) ->
    to_b(lists:flatten(list_to_hex(to_l(erlang:md5(S))))).

list_to_hex(Xs) ->
    [int_to_hex(X) || X <- Xs].

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0 + N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).

to_obj(Object) ->
    objectify(Object, [], list).

%% ===================================================================
%% Private
%% ===================================================================

objectify([], Acc, obj) ->
    {lists:reverse(Acc)};
objectify([], Acc, list)->
    lists:reverse(Acc);
objectify([{Key, Val} | Rest], Acc, _Type) when is_list(Val) ->
    objectify(Rest, [{Key, objectify(Val, [], list)} | Acc], obj);
objectify([{Key, Val} | Rest], Acc, _Type) when ?IS_PRIMITIVE(Val) ->
    objectify(Rest, [{Key, Val} | Acc], obj);
objectify([{Key, undefined} | Rest], Acc, _Type) ->
    objectify(Rest, [{Key, null} | Acc], obj);
objectify([{Key, Val} | Rest], Acc, _Type) ->
    objectify(Rest, [{Key, to_str(Val)} | Acc], obj);
objectify([Val| Rest], Acc, Type) when is_list(Val) ->
    objectify(Rest, [objectify(Val, [], list) | Acc], Type);
objectify([KV | Rest], Acc, Type) ->
    objectify(Rest, [KV | Acc], Type).

oneline(Str) ->
    replace(Str, <<"\n\s*">>, <<>>).

replace(Str, Old, New) ->
    re:replace(Str, Old, New, [global,{return,binary}]).

urlencode(Val) ->
    cowboy_http:urlencode(Val).

urldecode(Val) ->
    cowboy_http:urldecode(Val).
