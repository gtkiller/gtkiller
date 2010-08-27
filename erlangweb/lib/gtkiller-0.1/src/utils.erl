-module(utils).

-export([
    search_from_request_for/1,
    format/2, format/3,
    regexp_split_inclusive/2,

    % conversion helpers
    to_list/1, to_list/2,
    to_atom/1, to_atom/2,
    to_integer/1, to_integer/2,
    to_binary/1, to_binary/2,
    to_type/2, to_type/3,

    %dataflows
    check_admin_auth/2,

    % meta functions
    eval/2,

    % random strings
    random_char/1, random_string/0,
    random_string/1, random_string/2

]).

-include("constants.hrl").

%%
%% common simple search from json
%% request arguments "field" => "value"
%%
search_from_request_for(Table) ->
    Command = wpart:fget("_command"),

    RequestJSON = case Command of
        undefined -> jsonutils:decode(wpart:fget("post:request")); % for old API
        _ -> Command
    end,

    AttrName = jsonutils:get_attribute(RequestJSON, <<"field">>),
    AttrValue = jsonutils:get_attribute(RequestJSON, <<"value">>),

    (db:get_model_for(Table)):search(AttrName, AttrValue).


%%
%% gets type of a term
%% TODO: more checks
%%
get_type_of(Term) ->
    case Term of
        _integer when is_integer(Term) -> integer;
        _atom when is_atom(Term) -> atom;
        _list when is_list(Term) -> list;
        _binary when is_binary(Term) -> binary
    end.

%%
%% generic type converter
%% accepts Term and converts
%% it to needed Type
%%
to_type(Term, Type) ->
    to_type(Term, Type, Term).
to_type(Term, Type, Default) ->

    RealType = get_type_of(Term),

    if RealType =/= Type ->

        if

        % special binary case (only lists and atoms)
        Type =:= binary ->

            case RealType of
                list -> list_to_binary(Term);
                atom -> atom_to_binary(Term, utf8);
                _ -> Default
            end;

        % all other cases
        true ->

            % try to get correct integers from atoms and binaries, e.g. '1', <<"1">>
            if
                Type =:= integer, RealType =:= atom ->
                    FromType = list,
                    ConvertTerm = atom_to_list(Term);
                Type =:= integer, RealType =:= binary ->
                    FromType = list,
                    ConvertTerm = binary_to_list(Term);
                true ->
                    FromType = RealType,
                    ConvertTerm = Term
            end,

            ConvertBIF = list_to_atom(atom_to_list(FromType) ++ "_to_" ++ atom_to_list(Type)),

            case (catch erlang:(ConvertBIF)(ConvertTerm)) of
                {'EXIT', _} ->
                    console:log(["Cannot convert", Term, "to", Type, "return default", Default], 'ERROR'),
                    Default;
                Converted -> Converted % OK
            end

        end;

    true -> Term % no conversion is needed
    end.

%% conversion helpers

to_list(Term) ->
    to_list(Term, Term).
to_list(Term, Default) ->
    to_type(Term, list, Default).

to_atom(Term) ->
    to_atom(Term, Term).
to_atom(Term, Default) ->
    to_type(Term, atom, Default).

to_integer(Term) ->
    to_integer(Term, Term).
to_integer(Term, Default) ->
    to_type(Term, integer, Default).

to_binary(Term) ->
    to_binary(Term, Term).
to_binary(Term, Default) ->
    to_type(Term, binary, Default).

%%
%% common json formatter
%% TODO: optimize (general, but not efficient)
%%
format(Table, Data) ->
    format(Table, Data, {all, []}).

format(Table, Items, {FieldsCond, FieldsData}) when is_list(Items) ->
    [format(Table, Item, {FieldsCond, FieldsData}) || Item <- Items];

format(Table, Item, {FieldsCond, FieldsData}) ->

    % all table fields
    Fields = db:get_fields_of(Table),

    % query fields
    FormatFields = case FieldsCond of
        exclude -> Fields -- FieldsData;
        fields -> FieldsData;
        _ -> Fields
    end,

    JSONEncoded = lists:map(fun(X) ->

        Field = case X of
            _string when is_list(X) -> list_to_atom(X);
            _binary when is_binary(X) -> binary_to_atom(X, utf8);
            _ -> X
        end,

        V = element(db:get_attribute_index(Table, Field), Item),

        Value = case V of
            _stringV when is_list(V) -> list_to_binary(V);
            _dateV when is_tuple(V) -> list_to_binary(wtype_datetime:format(V)); % TODO: change for 'datetime' type check
            _ -> V
        end,

        {atom_to_binary(Field, utf8), Value}
    end, FormatFields),

    {struct, JSONEncoded}.

%-------------------------------------------------------------------------------
%dataflow funcs
%-------------------------------------------------------------------------------
check_admin_auth(_A, _B)->
    case e_auth:status() of
        true  -> {ok, _B};
        _     -> {error, not_logged_in}
    end.

regexp_loop(Str, Parts, Index, []) ->
    lists:reverse([string:substr(Str, Index)] ++ Parts);
regexp_loop(Str, Parts, Index, Rem_Matches) ->
    {NextPt,PtLen} = hd(Rem_Matches),
    regexp_loop( Str, [ string:substr(Str, NextPt, PtLen),
                        string:substr(Str, Index, NextPt - Index)]
                      ++ Parts, NextPt + PtLen,
                      tl(Rem_Matches) ).

regexp_split_inclusive(Str, Regex) ->
    {match, Matches} = regexp:matches(Str, Regex),
    regexp_loop(Str, [], 1, Matches).

%%
%% evaluates Erlang's Code;
%% variable bindings are taken
%% from Bindings
%%
eval(Code, Bindings) ->
  {ok, Scanned, _} = erl_scan:string(Code),
  {ok, Parsed} = erl_parse:parse_exprs(Scanned),
  EnvironmentRecord = lists:foldl(fun ({Key, Val}, BindingsAccumulator) ->
    erl_eval:add_binding(Key, Val, BindingsAccumulator)
  end, erl_eval:new_bindings(), Bindings),
  {value, Result, _} = erl_eval:exprs(Parsed, EnvironmentRecord),
  Result.

% random string functions

random_string() ->
    random_string(6).

random_string(Len) ->
    Digits = lists:seq(48, 57), % "0123456789"
    Letters = lists:seq(97,122), % "abcdefghijklmnopqrstuvwxyz"
    CapLetters = lists:seq(65, 90), % "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    random_string(Len, list_to_tuple(Digits ++ Letters ++ CapLetters)).

random_string(0, _Chars) -> [];
random_string(Len, Chars) -> [random_char(Chars) | random_string(Len - 1, Chars)].
random_char(Chars) -> element(random:uniform(tuple_size(Chars)), Chars).