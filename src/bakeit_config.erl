-module(bakeit_config).
-export([init/1, get/2]).

%%--------------------------------------------------------------------
init(Cfg) ->
    ets:new(bakeit_cfg, [named_table]),
    _ = [ets:insert(bakeit_cfg, KV) || KV <- Cfg],
    ok.

%%--------------------------------------------------------------------
get(Key) ->
    [{_, Val}] = ets:lookup(bakeit_cfg, Key),
    Val.

%%--------------------------------------------------------------------
get(Key, Default) ->
    case ets:lookup(bakeit_cfg, Key) of
        [{_, Val}] ->
            Val;
        [] ->
            Default
    end.

