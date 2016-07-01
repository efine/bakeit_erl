-module(bakeit_ini).
-export([read/0]).

-define(CFG_PATH, ".config/bakeit.cfg").

%%--------------------------------------------------------------------
read() ->
    ConfPath = filename:join(home_dir(), ?CFG_PATH),
    Config = load_from_file(ConfPath),
    Section = section(pastery, Config),
    ApiKey = ?MODULE:get(api_key, Section, pastery),
    [{api_key, ApiKey}].

%%--------------------------------------------------------------------
home_dir() ->
    case os:getenv("HOME") of
        false ->
            throw({error, <<"No HOME environment variable!">>});
        Dir ->
            Dir
    end.

%%--------------------------------------------------------------------
load_from_file(Filename) ->
    case zucchini:parse_file(Filename) of
        {ok, Config} ->
            Config;
        _Err ->
            Msg = ["Config file not found. Make sure you have a config file "
                   "at ~/", ?CFG_PATH, " with a [pastery] section containing "
                   "your Pastery API key, which you can get from your "
                   "https://www.pastery.net account page."],
            throw({error, list_to_binary(Msg)})
    end.

%%--------------------------------------------------------------------
section(Section, Config) ->
    case lists:keysearch(Section, 1, Config) of
        {value, {_, Props}} ->
            Props;
        _ ->
            Msg = io_lib:format(
                    "[~p] section not found. Please add a [~p] section to the "
                    "~s/~s file and try again.",
                    [Section, Section, "~", ?CFG_PATH]
                   ),
            throw({error, list_to_binary(Msg)})
    end.

%%--------------------------------------------------------------------
get(Key, Section, SectionName) ->
    case lists:keysearch(Key, 1, Section) of
        {value, {_, Val}} ->
            bakeit_util:to_s(Val);
        _ ->
            Msg = io_lib:format(
                    "No ~p entry found. Please add an entry for ~p to the "
                    "[~p] section with your API key in it. You can find the "
                    "latter on your account page on https://www.pastery.net.",
                    [Key, Key, SectionName]
                   ),
            throw({error, list_to_binary(Msg)})
    end.

