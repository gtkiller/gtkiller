-module(wtype_flag).
-behaviour(wtype).

-export([handle_call/2, validate/1]).

-include_lib("xmerl/include/xmerl.hrl").


%%
handle_call(_Format, #xmlText{value = Value}) ->
    #xmlText{value = handle_call(not_used, Value)};
handle_call(_Format, Value) when is_atom(Value) ->
    atom_to_list(Value);
handle_call(_Format, Value) ->
    Value.

%%
validate({Types, undefined}) ->
    case wpart_valid:is_private(Types) of
        true ->
            {ok, undefined};
        false ->
            case lists:keysearch(optional, 1, Types) of
                {value, {optional, Default}} ->
                    {ok, Default};
                _ ->
                    {ok, false}
            end
    end;
validate({Types, Input}) ->
    case wpart_valid:is_private(Types) of
        true ->
            {ok, Input};
        false ->
            {ok, not lists:member(Input, [false, "false", 0, ""])}
    end.
