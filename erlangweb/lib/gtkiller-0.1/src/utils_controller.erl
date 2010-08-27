-module(utils_controller).

-export([
    process_request/4,
    process_response/4,
    prepare_data/4,
    validate_data/4,
    check_existence/4,
    authorize/4
]).
-export([
    field_to_property/2, property_to_field/2,
    get_errors/0, clear_errors/0, add_error/1
]).

-include("lib/eptic-1.4.1/include/e_annotation.hrl").
-include("constants.hrl").


%%%
%%% Annotations.
%%%

%%
?BEFORE.
process_request(_Argument, _Module, _Function, Arguments) ->
    Type = string:substr(atom_to_list(hd(Arguments)), 12),
    Request = wpart:fget("post:request"),
    case Request of
        % Not-JSON request: return template.
        undefined ->
            {skip, {template, lists:append(Type, "/index.html")}};
        % JSON request: process request.
        _ ->
            JSON = mochijson2:decode(Request),
            console:log(["Request JSON:", JSON], 'DEBUG'),
            wpart:fset("_request", JSON),
            {proceed, Arguments}
    end.

%%
?AFTER.
process_response(_Argument, _Module, _Function, Result) ->
    JSON = wpart:fget("_response"),
    case JSON of
        % Not-JSON response: nothing to do.
        undefined ->
            {skip, Result};
        % JSON response: process response.
        _ ->
            Response = lists:flatten(mochijson2:encode(JSON)),
            console:log(["Response JSON:", Response], 'DEBUG'),
            {proceed, {content, text, Response}}
    end.

%%
?BEFORE.
prepare_data({Type}, _Module, Function, Arguments) ->
    Command = wpart:fget(?KEY_COMMAND),
    Item    = utils_validator:prepare_data(Type, Function, Command),
    wpart:fset(?KEY_ITEM, Item),
    {proceed, Arguments}.

%%
?BEFORE.
validate_data(_, Module, Function, Arguments) ->
    Data = wpart:fget(?KEY_ITEM),
    case utils_validator:validate_data(Data) of
        {ok, Item} ->
            wpart:fset(?KEY_ITEM, Item),
            {proceed, Arguments};
        {error, Errors} ->
            Callback = fun(Error) ->
                Field       = {field, atom_to_list(element(1, Error))},
                Description = {description, element(2, Error)},
                utils_error:insert({?ERROR_DATA_NOT_VALID, [Field, Description]})
            end,
            lists:foreach(Callback, Errors),
            {error, {Module, Function, [error]}}
    end.

%%
?BEFORE.
check_existence({Type, Action}, Module, _Function, Arguments) ->
    Command = wpart:fget("_command"),
    Id      = jsonutils:get_mochi_attribute(Command, <<"id">>),
    Model   = db:get_model_for(Type),
    case apply(Model, read, [Id]) of
        not_found ->
            add_error(item_not_exists),
            {error, {Module, Action, [error]}};
        _ ->
            {proceed, Arguments}
    end.

?BEFORE.
authorize(Format, _Mod, _Fun, Args) ->
    case e_auth:status() of
        true ->
            {proceed, Args};
        false ->
            case Format of
                json -> {skip, [{<<"errors">>, [{struct, [{<<"error">>, utils:to_integer(?ERROR_AUTH_REQUIRED)}, {<<"description">>, <<"Authorization required">>}]}]}]};
                json_old -> {skip, {content, text, jsonutils:encode({struct, [{<<"error">>, <<"Authorization required">>}]})}};
                _OtherFormat ->
                    wpart:fset("session:about2see", wpart:fget("__path")),
                    {skip, {redirect, "/login"}}
            end
    end.

%%%
%%% Utils.
%%%

%%
field_to_property(Type, Field) when is_atom(Type) ->
    field_to_property(atom_to_list(Type), Field);
field_to_property(Prefix, Field) ->
    lists:nthtail(length(Prefix) + 1, Field).

%%
property_to_field(Type, Property) when is_atom(Type) ->
    property_to_field(atom_to_list(Type), Property);
property_to_field(Prefix, Property) ->
    lists:append([Prefix, "_", Property]).

%%
get_errors() ->
    case wpart:fget("_errors") of
        undefined ->
            [];
        Errors ->
            Errors
    end.

%%
clear_errors() ->
    wpart:fset("_errors", []).

%%
add_error(Error) ->
    wpart:fset("_errors", get_errors() ++ [Error]).
