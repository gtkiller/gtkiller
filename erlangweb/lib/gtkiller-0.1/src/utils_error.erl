-module(utils_error).

-export([insert/1, select/0, clear/0]).
-export([get_code/1, get_description/1]).
-export([format/1]).

-include("../include/constants.hrl").


% Errors :: list(Error)
% Error :: {Code, list(Option)}
% Code :: atom()
% Option :: {Key, Value}
% Key :: atom()
% Value :: term()

%%
insert(Error) when is_atom(Error) ->
    insert({Error, []});
insert(Error) when (is_tuple(Error)) and (tuple_size(Error) == 1) ->
    insert(erlang:append_element(Error, []));
insert(Error) when (is_tuple(Error)) and (tuple_size(Error) == 2) ->
    wpart:fset(?KEY_ERRORS, [Error | select()]).

%%
select() ->
    case wpart:fget(?KEY_ERRORS) of
        undefined ->
            [];
        Errors ->
            Errors
    end.

%%
clear() ->
    wpart:fdelete(?KEY_ERRORS).

%%
get_code(Error) when (is_tuple(Error)) and ((tuple_size(Error) == 1) or (tuple_size(Error) == 2)) ->
    get_code(element(1, Error));
get_code(Error) when is_atom(Error) ->
    case (catch list_to_integer(atom_to_list(Error))) of
        {'EXIT', _} ->
            0;
        Code ->
            Code
    end.

%%
get_description(Error) when is_tuple(Error) and (tuple_size(Error) == 2) ->
    case lists:keyfind(description, 1, element(2, Error)) of
        false ->
            get_description(element(1, Error));
        {_, Description} ->
            get_description(Description)
    end;
get_description(Error) when is_tuple(Error) and (tuple_size(Error) == 1) ->
    get_description(element(1, Error));
get_description(Error) when is_atom(Error) ->
    e_lang:translate(errors, Error).

%%
format(Error) when is_atom(Error) ->
    format({Error, []});
format(Error) when (is_tuple(Error)) and (tuple_size(Error) == 1) ->
    format(erlang:append_element(Error, []));
format(Error) when (is_tuple(Error)) and (tuple_size(Error) == 2) ->
    Code        = get_code(Error),
    Description = utils:to_binary(get_description(Error)),
    Parameters  = [{?TO_JSON_NAME(K), utils:to_binary(V)} || {K, V} <- element(2, Error), (K /= code) and (K /= description)],
    {struct, [{?JSON_CODE, Code} | [{?JSON_DESCRIPTION, Description} | Parameters]]}.
