-module(bakeit_main).

%% API exports
-export([
         do_main/1,
         terminate/2
        ]).

-export_type([
              return_code/0,
              terminate_arg/0
             ]).

-import(bakeit_util, [msg/1, msg/2, err_msg/1, err_msg/2, to_s/1, req_prop/2]).

-define(APP_VERSION, "0.1").

-include("bakeit.hrl").

-type return_code() :: integer().
-type terminate_arg() :: 'help' | 'version' |
                         {error, string()} |
                         {exception, Class :: atom(), Reason :: term()} |
                         {return_code(), string()} |
                         return_code().

%%====================================================================
%% API functions
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

%%====================================================================
%% Internal functions
%%====================================================================
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
    bakeit_upload:go(Config);
run(Action, Config) ->
    msg("Action: ~p~nConfig:~n~p~n", [Action, Config]),
    {error, "Unhandled action: " ++ to_s(Action)}.

%%--------------------------------------------------------------------
-spec version(PgmName) -> ok when PgmName :: string().
version(PgmName) ->
    io:format(standard_error, "~s ~s~n", [PgmName, ?APP_VERSION]).

%%--------------------------------------------------------------------
-spec usage(PgmName) -> ok when PgmName :: string().
usage(PgmName) ->
    bakeit_cmdline:usage(PgmName),
    ok.

