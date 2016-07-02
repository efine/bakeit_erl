-module(bakeit_upload).

-export([go/1]).
-import(bakeit_util, [msg/1, msg/2, err_msg/1, err_msg/2, req_prop/2]).

-include("bakeit.hrl").

-define(PASTERY_URL, "https://www.pastery.net/api/paste/").

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
go(Config) ->
    Cfg = req_val(bakeit, Config),
    ok = bakeit_config:init(Cfg),
    Files = req_val(files, Config),
    BakeitCfg = bakeit_ini:read(),
    Data = get_data(Files),

    dbg_do(fun() -> dbg(Config, Cfg, Files, BakeitCfg, Data) end),

    {ok, _Apps} = application:ensure_all_started(bakeit),

    Request = make_request(#{data      => Data,
                             api_key   => req_val(api_key, BakeitCfg),
                             title     => title_or_filename(Cfg, Files),
                             language  => req_val(language, Cfg),
                             duration  => req_val(duration, Cfg),
                             max_views => req_val(max_views, Cfg)
                            }),
    msg("Uploading to Pastery...~n"),
    case upload(Request) of
        {ok, EJSON} ->
            Url = req_val(<<"url">>, EJSON),
            msg("Paste URL: ~s~n", [Url]),
            ok = maybe_launch_webbrowser(Url, Cfg),
            ?RC_SUCCESS;
        Err ->
            Err
    end.

%%--------------------------------------------------------------------
upload({_Url, _Headers, _ContentType, _Body} = Request) ->
    HttpOpts = [{ssl, []}],
    Opts = [{body_format, binary}],
    dbg_do(fun() -> httpc:set_options([{verbose, debug}]) end),
    post_upload_req(Request, HttpOpts, Opts).

%%--------------------------------------------------------------------
make_request(#{} = M) ->
    Body = maps:get(data, M),
    QueryParams = make_qps(M),
    Url = make_url(?PASTERY_URL, QueryParams),
    dbg_print("Url: ~s~n", [Url]),
    Headers = [ua_hdr(), content_length_hdr(Body)],
    ContentType = "application/octet-stream",
    {Url, Headers, ContentType, Body}.


%%--------------------------------------------------------------------
maybe_launch_webbrowser(Url, Cfg) ->
    _ = req_val(open_browser, Cfg) andalso launch_webbrowser(Url),
    ok.

%%--------------------------------------------------------------------
launch_webbrowser(<<Url/binary>>) ->
    launch_webbrowser(binary_to_list(Url));
launch_webbrowser(Url) ->
    case webbrowser:open(Url) of
        ok ->
            true;
        {error, {not_found, Msg}} ->
            err_msg(Msg),
            false
    end.

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
title_or_filename(Cfg, Files) ->
    case req_val(title, Cfg) of
        [] ->
            get_filename(Files);
        Title ->
            Title
    end.

%%--------------------------------------------------------------------
get_filename([Filename|_]) ->
    filename:basename(Filename);
get_filename([]) ->
    [].

%%--------------------------------------------------------------------
make_qps(#{} = M) ->
    [make_qp(api_key, M)] ++
    opt_qp(title, M, fun(X) -> X =/= [] end) ++
    opt_qp(language, M, fun(X) -> X =/= [] end) ++
    opt_qp(duration, M, fun(X) -> X > 0 end) ++
    opt_qp(max_views, M, fun(X) -> X > 0 end).

%%--------------------------------------------------------------------
make_qp(Key, #{} = M) ->
    make_kv(Key, maps:get(Key, M)).

%%--------------------------------------------------------------------
make_kv(Key, Val) ->
    {bakeit_util:to_s(Key), bakeit_util:to_s(Val)}.

%%--------------------------------------------------------------------
opt_qp(Key, #{} = M, Pred) ->
    Val = maps:get(Key, M),
    case Pred(Val) of
        true ->
            [make_kv(Key, Val)];
        false ->
            []
    end.

%%--------------------------------------------------------------------
make_url(Url, QPs) ->
    lists:flatten([Url, "?", encode_qps(QPs)]).

%%--------------------------------------------------------------------
encode_qps(QPs) ->
    encode_qps(QPs, []).

encode_qps([{K, V}], Acc) ->
    encode_qps([], [encode_qp(K, V) | Acc]);
encode_qps([{K, V}|T], Acc) ->
    encode_qps(T, [$&, encode_qp(K, V) | Acc]);
encode_qps([], Acc) ->
    lists:reverse(Acc).

encode_qp(K, V) ->
    [K, $=, http_uri:encode(V)].

%%--------------------------------------------------------------------
post_upload_req(Request, HttpOpts, Opts) ->
    case httpc:request(post, Request, HttpOpts, Opts) of
        {ok, Response} ->
            parse_upload_rsp(Response);
        Err ->
            Err
    end.

%%--------------------------------------------------------------------
parse_upload_rsp({{_Version, Status, Reason}, _Headers, Body}) ->
        dbg_print("Status: ~B~n"
                  "Reason: '~s'~n"
                  "Body:   ~P~n", [Status, Reason, Body, 512]),
        case Status of
            N when N >= 300, N < 400 ->
                Msg = ["Unexpected redirect: ", integer_to_list(N),
                       " ", Reason],
                {?RC_FATAL, to_b(Msg)};
            413 ->
                {?RC_FATAL, "The chosen file was rejected by the server "
                            "because it was too large, please try a smaller "
                            "file."};
            422 ->
                {?RC_FATAL, get_error_msg(Body)};
            N when N >= 400, N < 500 ->
                Msg = ["There was a problem with the request: ",
                       integer_to_list(N), " ", Reason],
                {?RC_FATAL, to_b(Msg)};
            N when N >= 500 ->
                Msg = ["There was a server error ", integer_to_list(N),
                       ", please try again later."],
                {?RC_FATAL, to_b(Msg)};
            _ ->
                {ok, parse_body(Body)}
        end.

%%--------------------------------------------------------------------
get_error_msg(Body) ->
    try
        EJSON = parse_body(Body),
        req_val(<<"error_msg">>, EJSON)
    catch
        _:_ ->
            Body
    end.

%%--------------------------------------------------------------------
parse_body(Body) ->
    jsx:decode(Body).

%%--------------------------------------------------------------------
dbg_print(Fmt) ->
    dbg_print(Fmt, []).

%%--------------------------------------------------------------------
dbg_print(Fmt, Args) ->
    dbg_do(fun() -> err_msg(Fmt, Args) end).

%%--------------------------------------------------------------------
dbg(Config, Cfg, Files, BakeitCfg, Data) ->
    dbg_do(fun() ->
                   _ = err_msg("Config:~n~p~n", [Config]),
                   _ = err_msg("Cfg:~n~p~n", [Cfg]),
                   _ = err_msg("Files: ~p~n", [Files]),
                   _ = err_msg("BakeitCfg: ~p~n", [BakeitCfg]),
                   _ = err_msg("Data:~n~s~n", [Data])
           end).

%%--------------------------------------------------------------------
dbg_do(Fun) when is_function(Fun, 0) ->
    bakeit_config:get(debug) andalso ?mktrue(Fun()).

%%--------------------------------------------------------------------
val({_, V}) -> V.

%%--------------------------------------------------------------------
req_val(K, PL) ->
    val(req_prop(K, PL)).

%%--------------------------------------------------------------------
to_b(<<B/binary>>) ->
    B;
to_b(S) when is_list(S) ->
    list_to_binary(S).

%%--------------------------------------------------------------------
content_length_hdr(<<Body/binary>>) ->
    {"Content-Length", integer_to_list(byte_size(Body))}.

%%--------------------------------------------------------------------
ua_hdr() ->
    {"User-Agent", "'Mozilla/5.0 (Erlang) bakeit library"}.

