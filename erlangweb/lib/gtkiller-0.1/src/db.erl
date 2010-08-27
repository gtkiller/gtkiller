-module(db).

-export([

    % migration helpers
    init/0,
    install/1,
    reset_accounts/0,

    % table/model info helpers
    get_fields_of/2,
    get_fields_of/1,
    get_model_for/1,
    get_table_info/1,
    get_fields_info/1,
    get_types_of/1,
    get_next_id_if/2,
    set_object_id/2,
    get_attribute_index/2,

    % db queries helpers
    select/2,
    foreach/3,
    search/3,
    get_first/3,
    exec/1,
    get_table_attribute/2,
    parse_filter_rules/3

]).

-define(TABS, [account]).

%% predicate operands
%% used in generic select
-define(OP_PREDICATE, [
    {<<"greater">>, <<">">>},
    {<<"greaterOrEqual">>, <<">=">>},
    {<<"equal">>, <<"=:=">>},
    {<<"notEqual">>, <<"=/=">>},
    {<<"lessOrEqual">>, <<"=<">>},
    {<<"less">>, <<"<">>}
]).

-include("account.hrl").

-include_lib("stdlib/include/qlc.hrl").

init() ->
    mnesia:stop(),
    Node1 = node(),
    NodesList = [Node1],
    case mnesia:create_schema(NodesList) of
        ok ->
            application:start(mnesia),
            e_db:install(),

            [install(Table) || Table <- ?TABS],

            reset_accounts();

        {error, {Node, {already_exists, Node} }}  ->
            application:start(mnesia),
            error_logger:warning_msg("~p module, mnesia's schema already exists, "
                                     "if you want to delete it, run mnesia:delete_schema/1~n",
                                     [?MODULE]),
            {error, schema_already_exists}
    end.

install(Name) ->
    mnesia:create_table(Name, [{attributes, (list_to_atom("wtype_" ++ atom_to_list(Name))):get_record_info(Name)},
                               {disc_copies, [node()]}]).

%% additional console helpers

reset_accounts() ->
    All = wtype_account:read(all),

    [wtype_account:delete(element(2, X)) || X <- All],

    ResetTable = fun(Table) ->
        mnesia:delete_table(Table),
        install(Table)
    end,

    [ResetTable(Table) || Table <- [account]],

    %create admin account for webaccess
    e_auth:add_user("admin", "olupadmin").

%%
%% gets model module
%% for mnesia table
%%
get_model_for(Table) ->
    list_to_atom("wtype_" ++ atom_to_list(Table)).

%%
%% gets fields of
%% mnesia table
%%
get_fields_of(Table) ->
    get_fields_of(Table, {exclude, []}).

get_fields_of(Table, {exclude, ExcludeFields}) ->
    (get_model_for(Table)):get_record_info(Table) -- ExcludeFields.

%%
%% gets field types of
%% mnesia table
%%
get_types_of(Table) ->
    (get_model_for(Table)):get_record_info(list_to_atom(atom_to_list(Table) ++ "_types")).

%%
%% gets attribute type
%%
get_attribute_type(Table, Attribute) ->
    Types = get_types_of(Table),
    element(1, element(get_attribute_index(Table, Attribute), Types)).

%%
%% returns value of an attribute from a tuple
%% at the type of the attribute
%%
get_attribute_index(Table, Attribute) ->
    CachedName = "__cached_index_" ++ utils:to_list(Table) ++ "_" ++ utils:to_list(Attribute),
    case get(CachedName) of
        undefined ->
            Index = xlists:index_of(utils:to_atom(Attribute), get_fields_of(Table)) + 1,
            put(CachedName, Index),
            Index;
        CachedIndex -> CachedIndex
    end.

cast_value_to_attribute_type(Table, Attribute, Value) ->
    Type = get_attribute_type(Table, Attribute),
    case Type of
        integer when is_list(Value) -> list_to_integer(Value);
        _ -> Value
    end.

%%
%% gets table info (fields, types)
%%
get_table_info(Table) ->
    {get_fields_of(Table), get_types_of(Table)}.

%% returns TFieldDefinition information
%% fields, their types and constraints
get_fields_info(Table) ->

    {Fields, Types} = get_table_info(Table),

    _NotUsed = [description, comment, private, optional],

    lists:map(fun(Field) ->

        {Type, Data} = element(get_attribute_index(Table, Field), Types),

        Constraints = [

            {struct, [
                {<<"method">>, atom_to_binary(Method, utf8)},
                {<<"data">>, utils:to_binary(Value)}
            ]}

            || {Method, Value} <- Data, not lists:member(Method, _NotUsed)
        ],

        BasicInfo = [
            {<<"name">>, atom_to_binary(Field, utf8)},
            {<<"type">>, atom_to_binary(Type, utf8)},
            {<<"constraints">>, Constraints}
        ],

        Info = if
            hd(Data) == primary_key -> BasicInfo ++ [{<<"primary">>, true}];
            true -> BasicInfo
        end,

        {struct, Info}

    end, Fields).

%%
%% generic select:
%% accepts table name atom
%% and match spec TFilterRule or
%% predicate function
%%

select(From, PredicateFunction) when is_function(PredicateFunction) ->
    exec(qlc:q([Record || Record <- mnesia:table(From), apply(PredicateFunction, [Record])]));

select(From, FilterRule) ->
    exec(qlc:q([
        Record || Record <- mnesia:table(From),
        % dynamic conditions
        utils:eval(parse_filter_rules(From, FilterRule, "") ++ ".", [{'R', Record}])
    ])).

foreach(From, PredicateFunction, MapFunction)
    when is_function(PredicateFunction), is_function(MapFunction) ->
    exec(qlc:q([MapFunction(Record) || Record <- mnesia:table(From), apply(PredicateFunction, [Record])])).

%%
%% parses TFilterRule match spec
%%
parse_filter_rules(_From, [], _M) -> "";
parse_filter_rules(_From, [D | Rules], M) ->
    MS = case M of
        "" -> "";
        _ -> " " ++ M ++ " "
    end,
    lists:flatten(["(" ++ parse_filter_rules(_From, D, M) ++ ")" ++ MS | parse_filter_rules(_From, Rules, "")]);
parse_filter_rules(From, {struct, D}, _M) ->
    Match = binary_to_list(proplists:get_value(<<"match">>, D)),
    case proplists:get_value(<<"rules">>, D) of
        undefined ->
            Field = "erlang:element(db:get_attribute_index(" ++ atom_to_list(From) ++ ", " ++ binary_to_list(proplists:get_value(<<"field">>, D)) ++ "), R)",
            Value = binary_to_list(proplists:get_value(<<"value">>, D)),
            get_match_spec(Match, Field, Value);
        _Rules ->
            parse_filter_rules(From, _Rules, Match)
    end.

get_match_spec(Match, Field, Value) ->
    F = fun (X) ->
        case X of
            _binary when is_binary(X) -> binary_to_list(X);
            _ -> X
        end
    end,
    QValue = "\"" ++ Value ++ "\"",
    case list_to_binary(Match) of
        <<"regexp">> -> wrap_spec("regexp:match", Field, QValue) ++ " =/= nomatch";
        <<"in">> -> wrap_spec("lists:member", Field, lists:map(F, Value));
        <<"begins">> -> wrap_spec("string:str", Field, QValue) ++ " == 1";
        <<"contains">> -> wrap_spec("string:str", Field, QValue) ++ " =/= 0";
        <<"ends">> -> wrap_spec("string:str", Field, QValue) ++ " - length(" ++ QValue ++ ") == 0";
        _Standard -> Field ++ binary_to_list(proplists:get_value(_Standard, ?OP_PREDICATE)) ++ QValue
    end.

wrap_spec(Wrapper, Field, Value) ->
    Wrapper ++ "("  ++ Field ++ ", " ++ Value ++ ")".

search(Table, Attribute, Value) ->

    SearchValue = cast_value_to_attribute_type(Table, Attribute, Value),
    AttributeIdx = get_attribute_index(Table, Attribute),

    try
        exec(qlc:q([X || X <- mnesia:table(Table), element(AttributeIdx, X) =:= SearchValue]))
    of
        Val -> Val
    catch
        _:_ -> false
    end.

get_table_attribute(Table, Attribute) ->
    AttributeIdx = get_attribute_index(Table, Attribute),
    exec(qlc:q([element(AttributeIdx, X) || X <- mnesia:table(Table)])).

get_first(Table, Attribute, Value) ->
    Res = search(Table, Attribute, Value),
    case Res of
        [] -> undefined;
        _Found -> hd(Res)
    end.

get_next_id_if(Table, Row) ->
    Id = element(2, Row),
    case Id of
        undefined -> e_db:get_next_id(Table);
        _string when is_list(Id) -> list_to_integer(Id);
        _ -> Id
    end.

set_object_id(Table, Row) ->
    NextId = get_next_id_if(Table, Row),
    setelement(2, Row, NextId).

% qcl query helper
exec(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
