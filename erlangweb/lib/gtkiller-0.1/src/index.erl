-module(index).
-export([index/1, user_index/1]).

-include("utils_controller_annotations.hrl").

?AUTHORIZE(normal).
index(_Args) ->
    UserId = wpart:fget("session:user_id"),
    CurrentJid = wpart:fget("session:currentjid"),
    CurrentAccountId = wpart:fget("session:account_id"),
    console:log(["INDEX:", UserId, CurrentJid, CurrentAccountId]),
    case CurrentAccountId of
        0 ->
			{template, "welcome.html"};
        _ ->

			{redirect, "/user_panel"}
    end.

?AUTHORIZE(normal).
user_index(_Args) ->
	Item = wtype_account:read(wpart:fget("session:account_id")),
	wpart:fset("account", wtype_account:format(Item)),
    {template, "welcome_user.html"}.

