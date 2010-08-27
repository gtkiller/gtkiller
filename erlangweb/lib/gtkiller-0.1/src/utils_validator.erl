-module(utils_validator).

-export([validate_data/1]).
-export([prepare_data/3]).

-include("../include/constants.hrl").


% Errors :: list(Error)
% Error :: {Field, Description}
% Filed :: atom()
% Description :: atom()

%%
validate_data(Data) ->
	Type        = element(1, Data),
	Id          = element(2, Data),
	Values      = [{ok, V} || V <- tl(tuple_to_list(Data))],
	Definitions = tl(tuple_to_list(db:get_types_of(Type))),
	Values0 = validate_primitives(Definitions, Values),
	Values1 = validate_uniqueness(Definitions, Values0, Type, Id),
	Fields = db:get_fields_of(Type),
	Errors = [{F, D} || {F, {error, {D, _}}} <- lists:zip(Fields, Values1)],
	case length(Errors) of
		0 ->
			{ok, list_to_tuple([Type | [V || {_, V} <- Values1]])};
		_ ->
			{error, Errors}
	end.

%%
validate_primitives(Definitions, Values) ->
	validate_primitives(Definitions, Values, []).

validate_primitives(_, [], Results) ->
    lists:reverse(Results);
validate_primitives([_ | Definitions], [{error, Value} | Values], Results) ->
	validate_primitives(Definitions, Values, [{error, Value} | Results]);
validate_primitives([{Primitive, Options} | Definitions], [{ok, Value} | Values], Results) ->
	Result = apply(list_to_atom("wtype_" ++ atom_to_list(Primitive)), validate, [{Options, Value}]),
	validate_primitives(Definitions, Values, [Result | Results]).

%%
validate_uniqueness(Definitions, Values, Type, Id) ->
	validate_uniqueness(Definitions, Values, Type, Id, []).

validate_uniqueness(_, [], _, _, Results) ->
    lists:reverse(Results);
validate_uniqueness([_ | Definitions], [{error, Value} | Values], Type, Id, Results) ->
	validate_uniqueness(Definitions, Values, Type, Id, [{error, Value} | Results]);
validate_uniqueness([{_, Options} | Definitions], [{ok, Value} | Values], Type, Id, Results) ->
	Result = case lists:member(unique, Options) of
		true ->
			check_uniqueness(Value, length(Results) + 2, Type, Id);
		false ->
			{ok, Value}
	end,
	validate_uniqueness(Definitions, Values, Type, Id, [Result | Results]).

check_uniqueness(Value, Position, Type, Id) ->
    Predicate = fun(Item) when (element(Position, Item) == Value) and (element(2, Item) /= Id) ->
			true;
	    (_) ->
			false
	end,
    case lists:any(Predicate, e_db:read(Type)) of
		true ->
			{error, {element_not_unique, Value}};
		false ->
			{ok, Value}
    end.

%%
prepare_data(Type, Action, Command) ->
	Data   = jsonutils:get_attribute(Command, ?JSON_DATA),
    Fields = db:get_fields_of(Type),
	Values = prepare_values(tl(Fields), Data),
	case Action of
		create ->
			list_to_tuple([Type | [undefined | Values]]);
		_ ->
			Id = jsonutils:get_attribute(Command, ?JSON_ID),
			list_to_tuple([Type | [Id | Values]])
	end.

prepare_values(Fields, Data) ->
	prepare_values(Fields, Data, []).

prepare_values([], _, Results) ->
    lists:reverse(Results);
prepare_values([Field | Fields], Data, Results) ->
	Result = jsonutils:get_attribute(Data, ?TO_JSON_NAME(Field)),
	prepare_values(Fields, Data, [Result | Results]).
