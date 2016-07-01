-module(bakeit).

%% API exports
-export([main/1]).
-import(bakeit_util, [msg/1, msg/2, err_msg/1, err_msg/2, to_s/1, req_prop/2]).

-define(APP_VERSION, "0.1").

-define(RC_SUCCESS, 0).
-define(RC_ERROR, 1).
-define(RC_FATAL, 2).

-type return_code() :: integer().
-type terminate_arg() :: 'help' | 'version' |
                         {error, string()} |
                         {exception, Class :: atom(), Reason :: term()} |
                         {return_code(), string()} |
                         return_code().

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
-spec main(Args) -> no_return() when Args :: [string()].
main(Args) ->
    PgmName = filename:basename(escript:script_name()),
    halt(terminate(PgmName, do_main(Args))).


%%====================================================================
%% Internal functions
%%====================================================================
-spec do_main(Args) -> Result when
      Args :: [string()], Result :: terminate_arg().
do_main(Args) ->
    try bakeit_cmdline:parse_args(Args) of
        {ok, {action_help, _Config}} ->
            help;
        {ok, {action_version, _Config}} ->
            version;
        {error, _Errmsg} = Err ->
            Err;
        {ok, {Action, Config}} ->
            try_run(Action, Config)
    catch
        Class:Reason ->
            err_msg("Class: ~p, Reason: ~p~n", [Class, Reason]),
            {exception, Class, Reason}
    end.

%%--------------------------------------------------------------------
-spec try_run(Action, Config) -> Result when
      Action :: bakeit_cmdline:action(),
      Config :: bakeit_cmdline:config(),
      Result :: terminate_arg().
try_run(Action, Config) ->
    try
        run(Action, Config)
    catch
        throw:{error, Msg} ->
            {error, Msg}
    end.

%%--------------------------------------------------------------------
-spec run(Action, Config) -> Result when
      Action :: bakeit_cmdline:action(),
      Config :: bakeit_cmdline:config(),
      Result :: terminate_arg().
run(action_default, Config) ->
    go(Config);
run(Action, Config) ->
    msg("Action: ~p~nConfig:~n~p~n", [Action, Config]),
    {error, "Unhandled action: " ++ to_s(Action)}.

%%--------------------------------------------------------------------
go(Config) ->
    Cfg = val(req_prop(bakeit, Config)),
    init_config(Cfg),
    Files = val(req_prop(files, Config)),
    BakeitCfg = bakeit_config:read(),
    Data = get_data(Files),

    _ = is_debug() andalso dbg(Config, Cfg, Files, BakeitCfg, Data),

    {ok, _Apps} = application:ensure_all_started(bakeit),

    msg("Uploading to Pastery...~n"),
    Response = upload(#{data      => Data,
                        api_key   => val(req_prop(api_key, BakeitCfg)),
                        title     => title_or_filename(Cfg, Files),
                        language  => val(req_prop(language, Cfg)),
                        duration  => val(req_prop(duration, Cfg)),
                        max_views => val(req_prop(max_views, Cfg))
                       }),

    case Response of
        {ok, EJSON} ->
            URL = val(req_prop(<<"url">>, EJSON)),
            msg("Paste URL: ~s~n", [URL]),
            ?RC_SUCCESS;
        Err ->
            Err
    end.

%%--------------------------------------------------------------------
-spec terminate(ScriptName, Arg) -> integer() when
      ScriptName :: string(),
      Arg :: terminate_arg().
terminate(ScriptName, help) ->
    usage(ScriptName),
    terminate(ScriptName, ?RC_ERROR);
terminate(ScriptName, version) ->
    version(ScriptName),
    terminate(ScriptName, ?RC_ERROR);
terminate(ScriptName, {error, Errmsg}) ->
    usage(ScriptName),
    err_msg("***** ~s~n~n", [Errmsg]),
    terminate(ScriptName, ?RC_ERROR);
terminate(ScriptName, {exception, Class, Reason}) ->
    err_msg("***** ~p:~n~p~n", [Class, Reason]),
    err_msg("~p~n~n", [erlang:get_stacktrace()]),
    terminate(ScriptName, ?RC_FATAL);
terminate(ScriptName, {RC, Errmsg}) when is_integer(RC) ->
    err_msg("***** ~s~n~n", [Errmsg]),
    terminate(ScriptName, RC);
terminate(_ScriptName, RC) when is_integer(RC) ->
    RC.

%%--------------------------------------------------------------------
-spec usage(PgmName) -> ok when PgmName :: string().
usage(PgmName) ->
    bakeit_cmdline:usage(PgmName),
    ok.

%%--------------------------------------------------------------------
-spec version(PgmName) -> ok when PgmName :: string().
version(PgmName) ->
    io:format(standard_error, "~s ~s~n", [PgmName, ?APP_VERSION]).

%%--------------------------------------------------------------------
get_data(Files) ->
    case Files of
        [] ->
            read_stdin();
        [File] ->
            {ok, B} = file:read_file(File),
            B
    end.

%%--------------------------------------------------------------------
read_stdin() ->
    ok = io:setopts([{binary, true}, {encoding,unicode}]),
    read_stdin([]).

%%--------------------------------------------------------------------
read_stdin(IOList) ->
    case io:get_chars("", 8192) of
        eof ->
            list_to_binary(lists:reverse(IOList));
        <<B/binary>> ->
            read_stdin([B | IOList])
    end.

%%--------------------------------------------------------------------
title_or_filename(Cfg, []) ->
    val(req_prop(title, Cfg));
title_or_filename(_Cfg, [Filename]) ->
    filename:basename(Filename).

upload(#{} = M) ->
    Body = maps:get(data, M),
    QueryParams = make_qps(M),
    Url = make_url("https://www.pastery.net/api/paste/", QueryParams),
    dbg_print("Url: ~s~n", [Url]),
    Headers = [{"User-Agent", "'Mozilla/5.0 (Erlang) bakeit library"},
               {"Content-Length", integer_to_list(byte_size(Body))}],
    ContentType = "application/octet-stream",
    Request = {Url, Headers, ContentType, Body},
    HttpOpts = [{ssl, []}],
    Opts = [{body_format, binary}],
    dbg_do(fun() -> httpc:set_options([{verbose, debug}]) end),
    case httpc:request(post, Request, HttpOpts, Opts) of
        {ok, Response} ->
            parse_response(Response);
        Err ->
            Err
    end.


make_qps(#{} = M) ->
    [make_qp(api_key, M)] ++
    opt_qp(title, M, fun(X) -> X =/= [] end) ++
    opt_qp(language, M, fun(X) -> X =/= [] end) ++
    opt_qp(duration, M, fun(X) -> X > 0 end) ++
    opt_qp(max_views, M, fun(X) -> X > 0 end).

make_qp(Key, #{} = M) ->
    make_kv(Key, maps:get(Key, M)).

make_kv(Key, Val) ->
    {to_s(Key), to_s(Val)}.

opt_qp(Key, #{} = M, Pred) ->
    Val = maps:get(Key, M),
    case Pred(Val) of
        true ->
            [make_kv(Key, Val)];
        false ->
            []
    end.

make_url(Url, QPs) ->
    lists:flatten([Url, "?", encode_qps(QPs)]).

encode_qps(QPs) ->
    string:join([Key ++ "=" ++ Val || {Key, Val} <- QPs], "&").

parse_response({{_Version, Status, Reason}, _Headers, Body}) ->
        dbg_print("Status: ~B~n"
                  "Reason: '~s'~n"
                  "Body:   ~P~n", [Status, Reason, Body, 512]),
        case Status of
            N when N >= 300, N < 400 ->
                Msg = ["Unexpected redirect: ", integer_to_list(N),
                       " ", Reason],
                {error, lists:flatten(Msg)};
            413 ->
                {error, "The chosen file was rejected by the server because "
                        "it was too large, please try a smaller file."};
            422 ->
                {error, get_error_msg(Body)};
            N when N >= 400, N < 500 ->
                Msg = ["There was a problem with the request: ",
                       integer_to_list(N), " ", Reason],
                {error, lists:flatten(Msg)};
            N when N >= 500 ->
                Msg = ["There was a server error ", integer_to_list(N),
                       ", please try again later."],
                {error, lists:flatten(Msg)};
            _ ->
                {ok, parse_body(Body)}
        end.

get_error_msg(Body) ->
    try
        EJSON = parse_body(Body),
        to_s(val(req_prop(<<"error_msg">>, EJSON)))
    catch
        _:_ ->
            to_s(Body)
    end.

parse_body(Body) ->
    jsx:decode(Body).

is_debug() ->
    get_config(debug).

dbg_print(Fmt) ->
    dbg_print(Fmt, []).

dbg_print(Fmt, Args) ->
    dbg_do(fun() -> err_msg(Fmt, Args) end).

dbg(Config, Cfg, Files, BakeitCfg, Data) ->
    dbg_do(fun() ->
                   err_msg("Config:~n~p~n", [Config]),
                   err_msg("Cfg:~n~p~n", [Cfg]),
                   err_msg("Files: ~p~n", [Files]),
                   err_msg("BakeitCfg: ~p~n", [BakeitCfg]),
                   err_msg("Data:~n~s~n", [Data]),
                   true
           end).

dbg_do(Fun) when is_function(Fun, 0) ->
    is_debug() andalso begin _ = Fun(), true end.

get_config(Key) ->
    [{_, Val}] = ets:lookup(bakeit_cfg, Key),
    Val.

init_config(Cfg) ->
    ets:new(bakeit_cfg, [named_table]),
    _ = [ets:insert(bakeit_cfg, KV) || KV <- Cfg],
    ok.

val({_, V}) -> V.

