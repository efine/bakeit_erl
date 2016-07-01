-module(bakeit_cmdline).

-export([
         option_spec_list/0,
         parse_args/1,
         usage/1
        ]).

%% Internal exports
-export([
         assert_or_die_fun/2
        ]).

-export_type([action/0, config/0]).

-type opt_specs() :: [getopt:option_spec()].
-type options() :: [getopt:option()].
-type action() :: atom().
-type config() :: proplists:proplist().

-spec option_spec_list() -> opt_specs().
option_spec_list() ->
    [
     {title,         $t,        "title",          {string, ""},       "The title of the paste"},
     {language,      $l,        "lang",           {string, ""},       "The language highlighter to use"},
     {duration,      $d,        "duration",       {integer, 60},      "The duration the paste should live for"},
     {max_views,     $v,        "max-views",      {integer, 0},       "How many times the paste can be viewed before it expires"},
     {open_browser,  $b,        "open-browser",   {boolean, false},   "Automatically open a browser window when done"},
     {debug,         $D,        "debug",          {boolean, false},   "Show debug info"},
     {help,          $h,        "help",           undefined,          "Show this screen"},
     {version,       $V,        "version",        undefined,          "Show version"}
    ].

-spec parse_args(Args) -> Result when
      Args :: [string()], Result :: {ok, {Action, Config}} | {error, ErrorText},
      Action :: atom(), Config :: proplists:proplist(), ErrorText :: string().
parse_args(Args) ->
    OptSpecList = option_spec_list(),
    Result = case getopt:parse(OptSpecList, Args) of
                 {ok, {Opts, NonOpts}} ->
                     show_parse_results(Opts, NonOpts),
                     make_action_cfg(OptSpecList, Opts, NonOpts);
                 Error ->
                     Error
             end,
    wrap_result(OptSpecList, Result).

usage(PgmName) ->
    getopt:usage(option_spec_list(), PgmName,
                 "[<filename>]",
                 [{"<filename>", "Input file, or omit for stdin"}]).

%%%====================================================================
%%% Internal functions
%%%====================================================================
-spec make_action_cfg(OptSpecList, Opts, NonOpts) -> Result when
      OptSpecList :: opt_specs(), Opts :: options(), NonOpts :: [string()],
      Result :: {ok, {Action, Config}} | {error, Reason},
      Action :: action(), Config :: config(), Reason :: term().
make_action_cfg(OptSpecList, Opts, NonOpts) ->
    case immediate_action(Opts) of
        {ok, Action} ->
            {ok, {Action, []}};
        undefined ->
            make_checked_action_cfg(OptSpecList, Opts, NonOpts)
    end.

-spec make_checked_action_cfg(OptSpecList, Opts, NonOpts) -> Result when
      OptSpecList :: opt_specs(), Opts :: options(), NonOpts :: [string()],
      Result :: {ok, {Action, Config}} | {error, Reason},
      Action :: action(), Config :: config(), Reason :: term().
make_checked_action_cfg(OptSpecList, Opts, NonOpts) ->
    case getopt:check(OptSpecList, Opts) of
        ok ->
            make_action_cfg(Opts, NonOpts);
        Error ->
            Error
    end.

-spec make_action_cfg(Opts, NonOpts) -> Result when
      Opts :: options(), NonOpts :: [string()],
      Result :: {ok, {Action, Config}} | {error, Reason},
      Action :: action(), Config :: config(), Reason :: term().
make_action_cfg(Opts, NonOpts) ->
    try
        Action = get_action(Opts),
        BakeitCfg = make_bakeit_cfg(Opts),
        FileCfg = make_file_cfg(NonOpts),
        Cfg = [{bakeit, BakeitCfg}, {files, FileCfg}],
        {ok, {Action, Cfg}}
    catch
        throw:Error ->
            {error, Error}
    end.

-spec immediate_action(Opts) -> Result
    when Opts :: options(), Result :: {ok, Action :: atom()} | undefined.
immediate_action(Opts) ->
    Immed = [{help, action_help},
             {version, action_version}],
    case [Action || {Opt, Action} <- Immed, lists:member(Opt, Opts)] of
        [A | _] -> {ok, A};
        _       -> undefined
    end.

-spec get_action(Opts) -> Result when
      Opts :: options(), Result :: action().
get_action(_Opts) ->
    action_default.

make_bakeit_cfg(Opts) ->
    [
     title(Opts),
     open_browser(Opts),
     max_views(Opts),
     duration(Opts),
     language(Opts),
     debug(Opts)
    ].

make_file_cfg([]) ->
    [];
make_file_cfg(["-"]) ->
    [standard_input];
make_file_cfg([Filename]) ->
    filelib:is_regular(Filename) orelse throw({not_regular_file, Filename}),
    [Filename];
make_file_cfg(_) ->
    throw(max_one_filename).

title(Opts) ->
    Pred = fun(V) -> io_lib:printable_unicode_list(V) end,
    assert_prop(Pred, title, Opts).

open_browser(Opts) ->
    lists:foldl(fun({open_browser, _} = V, _Acc) -> V;
                   (_, Acc) -> Acc
                end, bakeit_util:req_prop(open_browser, Opts), Opts).

max_views(Opts) ->
    Pred = fun(V) -> is_integer_range(V, 0, 16#FFFF) end,
    assert_prop(Pred, max_views, Opts).

duration(Opts) ->
    Pred = fun(V) -> is_integer_range(V, 0, 7 * 24 * 60) end,
    assert_prop(Pred, duration, Opts).

language(Opts) ->
    Pred = fun(V) -> io_lib:printable_unicode_list(V) end,
    assert_prop(Pred, language, Opts).

debug(Opts) ->
    bakeit_util:req_prop(debug, Opts).


assert_prop(Pred, Key, Props) when is_function(Pred, 1), is_list(Props) ->
    Prop = bakeit_util:req_prop(Key, Props),
    Exc = {invalid_option_arg, Prop},
    bakeit_util:map_prop(assert_or_die_fun(Pred, Exc), Prop).

assert_or_die_fun(Pred, Exc) ->
    fun(V) ->
            case Pred(V) of
                true  -> V;
                false -> throw(Exc)
            end
    end.

-spec is_integer_range(X, Min, Max) -> boolean()
    when X :: term(), Min :: integer(), Max :: integer().
is_integer_range(X, Min, Max) ->
    is_integer(X) andalso
    is_integer(Min) andalso
    is_integer(Max) andalso
    Max >= Min andalso
    X >= Min andalso
    X =< Max.

-spec wrap_result(OptSpecList, Result) -> WrappedResult when
      OptSpecList :: opt_specs(), Result :: OkResult | {error, term()},
      WrappedResult ::  OkResult | {error, nonempty_string()},
      OkResult :: {ok, term()}.
wrap_result(_OptSpecList, {ok, _} = Result) ->
    Result;
wrap_result(_OptSpecList, {error, {not_regular_file, Filename}}) ->
    {error, lists:flatten(["Not a regular file: `", Filename, "`"])};
wrap_result(_OptSpecList, {error, max_one_filename}) ->
    {error, "Only one file can be specified"};
wrap_result(_OptSpecList, Text) when is_binary(Text) ->
    {error, Text};
wrap_result(OptSpecList, Error) ->
    {error, lists:flatten(getopt:format_error(OptSpecList, Error))}.

show_parse_results(Opts, NonOpts) ->
    case proplists:get_value(debug, Opts) of
        true ->
            bakeit_util:msg("Parse results:~nOpts: ~p~nNonOpts: ~p~n",
                            [Opts, NonOpts]);
        _ ->
            ok
    end.

