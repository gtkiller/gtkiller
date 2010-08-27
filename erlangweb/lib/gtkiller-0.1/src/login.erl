-module(login).
-export([login/1, logout/1, json_login/1, is_user/1]).

-include("lib/gtkiller-0.1/include/account.hrl").

-include_lib("stdlib/include/qlc.hrl").

login(_Args) ->
    Username = wpart:fget("post:username"),
    Password = wpart:fget("post:password"),
    case e_auth:login(Username, Password) of
        ok ->
            wpart:fset("session:currentjid", Username),
            case db:get_first(account, jid, Username) of
                undefined -> console:log(["login32"]),wpart:fset("session:account_id", 0);
                CurrentUser ->
                    wpart:fset("session:account", CurrentUser),
                    wpart:fset("session:account_id", CurrentUser#account.id)
            end,
            case wpart:fget("session:about2see") of
    		undefined ->
                    {redirect, "/"};
    		URL ->
    		    wpart:finsert("session:about2see", undefined),
    		    {redirect, [$/ | URL]}
    	    end;
        {error, Reason} ->
            wpart:fset("error", Reason),
            {template, "login/unsuccessful.html"}
    end.

logout(_Args) ->
    e_auth:logout(),
    {redirect, "/"}.


json_login(_Args) ->

    RequestJSON = jsonutils:decode(wpart:fget("post:request")),

    Username    = jsonutils:get_attribute(RequestJSON, <<"username">>),
    Password    = jsonutils:get_attribute(RequestJSON, <<"password">>),
    Fingerprint = jsonutils:get_attribute(RequestJSON, <<"fingerprint">>),

    Credential = case Password of
        undefined -> Fingerprint; % fingerprint auth
        _ -> Password
    end,

    console:log(["Username:", Username, "Password:", Password, "Fingerprint:", Fingerprint, "Credential:", Credential]),

    case e_auth:login(Username, Credential) of

        ok ->
            wpart:fset("session:currentjid", Username),
            e_umschatserver:new_client_register(Username),

            case db:get_first(account, jid, Username) of
                undefined ->
                    wpart:fset("session:account_id", 0);
                Account ->
                    wpart:fset("session:account", Account),
                    wpart:fset("session:account_id", Account#account.id),
                    e_longpoll:register_event()
            end,

            console:log('Login is OK'),
            {content, text, "OK"};

        {error, Reason} ->
            console:log(["Login error:", Reason]),
            {content, text, "Failed - " ++ jsonutils:encode(Reason)}
    end.

is_user(_Args) ->
    RequestJSON = jsonutils:decode(wpart:fget("post:request")),
    Username    = jsonutils:get_attribute(RequestJSON, <<"username">>),

    case db:get_first(account, jid, Username) of
        undefined -> {content, text, "Failed"};
        _ -> {content, text, "OK"}
    end.

