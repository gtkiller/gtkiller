-module(admin).
-export([b2bua_console/1, user_manage/1]).

-include("utils_controller_annotations.hrl").

?AUTHORIZE(normal).
b2bua_console(_Args) ->
    {template, "admin/console.html"}.

?AUTHORIZE(normal).
user_manage(_Args) ->
    {template, "admin/user_manage.html"}.

