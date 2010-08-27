-module(controller).

-export([behaviour_info/1]).
-export([
    handle/0, handle/1,
    select/0, select/1,
    create/0, create/1,
    update/0, update/1,
    delete/0, delete/1
]).

-include("utils_controller_annotations.hrl").
-include("constants.hrl").


%%
behaviour_info(callbacks) ->
    [{handle, 1}];
behaviour_info(_) ->
    undefined.

%%%
%%% Request handler.
%%%

%%
handle() ->
    handle(get_controller()).

%%
?PROCESS_REQUEST({}).     % Before.
?PROCESS_RESPONSE({}).    % After.
handle(Controller) ->
    Request  = wpart:fget(?KEY_REQUEST),
    Commands = jsonutils:get_attribute(Request, ?JSON_COMMANDS),
    case length(Commands) of
        0 ->
            console:log(["Commands not found"], 'WARN');
        _ ->
            console:log(["Commands found:", Commands], 'DEBUG')
    end,
    Results  = execute_commands(Controller, Commands),
    Response = {struct, [{?JSON_COMMANDS, Results}]},
    wpart:fset(?KEY_RESPONSE, Response).

%%
execute_commands(_Controller, []) ->
    [];
execute_commands(Controller, [Command | Commands]) ->
    wpart:fset(?KEY_COMMAND, Command),
    Action  = list_to_atom(jsonutils:get_attribute(Command, ?JSON_TYPE)),
    Exports = Controller:module_info(exports),
    Result  = case lists:member({Action, 0}, Exports) of
        true ->
            console:log(["Action found:", {Controller, Action}], 'DEBUG'),
            apply(Controller, Action, []);
        _ ->
            console:log(["Action not found:", {Controller, Action}], 'WARN'),
            utils_error:insert(?ERROR_COMMAND_UNKNOWN),
            execute(error)
    end,
    wpart:fdelete(?KEY_COMMAND),
    [{struct, [{?JSON_TYPE, utils:to_binary(Action)} | Result]} | execute_commands(Controller, Commands)].

%%%
%%% Actions.
%%%

%%
select() ->
    select_with(get_type()).

select(error) ->
    select_with(get_type(), error).

select_with(Type) ->
    Model = db:get_model_for(Type),
    Command = wpart:fget(?KEY_COMMAND),

    console:log(["Command:", Command]),

    % list of needed fields in response
    FieldsData  = case jsonutils:get_attribute(Command, <<"fields">>) of
        undefined -> {exclude, []};
        Fields -> {fields, Fields}
    end,

    % main select result
    console:log(["Filter:", jsonutils:get_attribute(Command, <<"filter">>)]),
    Rows = case jsonutils:get_attribute(Command, <<"filter">>) of
        undefined -> apply(Model, read, [all]); % all data
        Filter -> db:select(Type, Filter)
    end,

    Data = [{<<"data">>, utils:format(Type, Rows, FieldsData)}],

    % field definitions and constraints
    case jsonutils:get_attribute(Command, <<"needFields">>) of
        undefined -> Data;
        false -> Data;
        true -> Data ++ [{<<"fields">>, db:get_fields_info(Type)}]
    end.

select_with(_Type, error) ->
    [].

%%
create() ->
    create_with(get_type()).

create(error) ->
    create_with(get_type(), error).

create_with(Type) ->
    Model = db:get_model_for(Type),
    Item  = apply(Model, create, [setelement(2, wpart:fget("_item"), undefined)]),
    [{<<"data">>, utils:format(Type, Item)}].

create_with(_, error) ->
    execute(error).

%%
update() ->
    update_with(get_type()).

update(error) ->
    update_with(get_type(), error).

update_with(Type) ->
    Model   = db:get_model_for(Type),
    Command = wpart:fget(?KEY_COMMAND),
    Item    = apply(Model, update, [setelement(2, wpart:fget("_item"), jsonutils:get_attribute(Command, <<"id">>))]),
    Id      = element(2, Item),
    [{<<"id">>, Id}, {<<"data">>, utils:format(Type, Item, {exclude, [id]})}].

update_with(_, error) ->
    execute(error).

%%
delete() ->
    delete_with(get_type()).

delete(error) ->
    delete_with(get_type(), error).

delete_with(Type) ->
    Command = wpart:fget(?KEY_COMMAND),
    Id      = jsonutils:get_mochi_attribute(Command, <<"id">>),
    Model   = db:get_model_for(Type),
    apply(Model, delete, [Id]),
    console:log(["Item deleted:", Id], 'DEBUG'),
    [{<<"id">>, Id}].

delete_with(_Type, error) ->
    execute(error).

%%
execute(error) ->
    Errors = utils_error:select(),
    utils_error:clear(),
    [{?JSON_ERRORS, [utils_error:format(E) || E <- Errors]}].

%%%
%%% Utils.
%%%

%%
get_controller() ->
    wpart:fget("__controller").

%%
get_type()->
    get_type_for(get_controller()).

%%
get_type_for(Controller)->
    list_to_existing_atom(lists:nthtail(11, atom_to_list(Controller))).
