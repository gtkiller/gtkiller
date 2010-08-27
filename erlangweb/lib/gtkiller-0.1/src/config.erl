-module(config).
-export([
    get/1,
    as_json/1
]).

get(Key) ->
    case e_conf:get_conf(Key) of
        undefined ->
            console:log(["Value for config key is not found:", Key]),
            undefined;
        Value -> Value
    end.

as_json(_Args) ->
    {content, text, jsonutils:encode(
        {struct, [
            {<<"ejabberd_host">>, list_to_binary(config:get(ejabberd_host))}
        ]}
    )}.