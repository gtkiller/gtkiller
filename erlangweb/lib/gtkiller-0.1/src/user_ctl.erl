-module(user_ctl).
-export([make_call/1, do_make_call/1, check_number/1]).

-include("utils_controller_annotations.hrl").

?AUTHORIZE(normal).
make_call(_Args) ->
    {template, "user/panel.html"}.
    
?AUTHORIZE(normal).
do_make_call(_Args) ->
    RequestJSON = jsonutils:decode(wpart:fget("post:request")),
    From = jsonutils:get_attribute(RequestJSON, <<"from">>),
    To = jsonutils:get_attribute(RequestJSON, <<"to">>),
    case check_number(From) of
	true ->
	    case check_number(To) of
		true -> {content, text, e_socat:command("c " ++ From ++ " " ++ To)};
		_ -> {content, text, "bad to number"}
	    end;
	_ -> {content, text, "bad from number"}
    end.

check_number(Number) ->
    case re:run(Number, "^[0-9]+$", [global, {capture,[1]}, {newline,any}]) of
            {match, _} -> true;
            _ -> false
    end.