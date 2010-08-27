%% Extra functions for lists

-module(xlists).

-export([
    index_of/2, index_of/3,
    is_nth/2, is_nth/3
]).


%% returns index of an element
%% with Value in List, starting
%% from the From position

index_of(Value, List) ->
    index_of(Value, List, 1).

index_of(_, [], _) -> 0;
index_of(Value, [H | T], From) ->
    case H == Value of
        true -> From;
        false -> index_of(Value, T, From + 1)
    end.

%% the same as index_of
%% but boolean result

is_nth(Value, List) ->
    is_nth(Value, List, 1).

is_nth(Value, List, From) ->
    case index_of(Value, List, From) of
        0 -> false;
        _ -> true
    end.