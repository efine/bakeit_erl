-module(bakeit_util).
-export([
         err_msg/1,
         err_msg/2,
         msg/1,
         msg/2,
         to_s/1,
         map_prop/2,
         req_prop/2
        ]).

err_msg(Fmt) ->
    err_msg(Fmt, []).

err_msg(Fmt, Args) ->
    {TsFmt, TsArgs} = add_ts(Fmt, Args),
    io:format(standard_error, TsFmt, TsArgs).

msg(Fmt) ->
    msg(Fmt, []).

msg(Fmt, Args) ->
    {TsFmt, TsArgs} = add_ts(Fmt, Args),
    io:format(TsFmt, TsArgs).

to_s(X) when is_list(X)    -> X;
to_s(X) when is_atom(X)    -> atom_to_list(X);
to_s(X) when is_integer(X) -> integer_to_list(X);
to_s(X) when is_binary(X)  -> binary_to_list(X);
to_s(X)                    -> lists:flatten(io_lib:format("~p", [X])).

map_prop(Fun, {K, V}) when is_function(Fun, 1) ->
    {K, Fun(V)}.

req_prop(K, PL) ->
    case lists:keysearch(K, 1, PL) of
        {value, KV} ->
            KV;
        false ->
            throw({missing_required_key, K})
    end.

add_ts(Fmt, Args) ->
    {"[~s] " ++ Fmt, [iso8601_ts() | Args]}.

iso8601_ts() ->
    Now = os:timestamp(),
    Micros = element(3, Now),
    {{Yr, Mo, Dy}, {H, M, S}} = calendar:now_to_universal_time(Now),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~6..0BZ",
                  [Yr, Mo, Dy, H, M, S, Micros]).

