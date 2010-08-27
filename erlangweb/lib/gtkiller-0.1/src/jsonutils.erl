-module(jsonutils).

-export([
    encode/1,
    decode/1,

    get_attribute/2,
    get_mochi_attribute/2,

    test_mochi/0
]).

encode(MochiJSON) ->
    Encode = mochijson2:encoder([{utf8, true}]),
    Encode(MochiJSON).

decode(JSON) ->
    mochijson2:decode(JSON).

get_attribute(JSON, Attribute) ->
    F = fun(X) ->
        case(X) of
            X when is_binary(X) -> binary_to_list(X);
            X -> X
        end end,
    case get_mochi_attribute(JSON, Attribute) of
	    _list when is_list(_list) -> lists:map(F, _list);
	    _encodedString when is_binary(_encodedString) -> binary_to_list(_encodedString); % quick
        X -> X
    end.

get_mochi_attribute([], _) -> undefined;
get_mochi_attribute(S, Attr) ->
    F = fun(X)-> X =/= undefined end,
    case S of
        {struct, Value} -> get_mochi_attribute(Value, Attr);
        [{Name, Value}|_T] when Name =:= Attr -> Value;
        [{Name, Value}] when Name =:= Attr -> Value;
        [{_Name, Value}|T] when is_binary(Value);
                    is_atom(Value); is_number(Value) ->
            get_mochi_attribute(T, Attr);
        [{_Name, _Value}|T] ->
            lists:append(lists:filter(F,
                [get_mochi_attribute(_Value, Attr), get_mochi_attribute(T, Attr)]));
        _A ->  console:log(["Unknown struct:", _A], 'ERROR'), undefined
    end.

test_mochi() ->
    D = {struct,
      [{<<"type">>,<<"select">>},
       {<<"needFields">>,true},
       {<<"needRules">>,true},
       {<<"needTotalSize">>,true},
       {<<"filters">>,[]},
       {<<"orderBy">>,
        [{struct,[{<<"field">>,<<"owner_last_name">>}]},
         {struct,[{<<"field">>,<<"owner_first_name">>}]}]},
       {<<"page">>,
        {struct,
            [{<<"index">>,1},
             {<<"size">>,100},
             {<<"where">>,null}]}},
       {<<"source">>,<<"users">>}]},
    V = get_mochi_attribute(D, <<"source">>),
    V.

