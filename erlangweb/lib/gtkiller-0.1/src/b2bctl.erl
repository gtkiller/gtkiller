-module(b2bctl).

-export([ testreq/1 ]).

testreq(_Args) ->
    RequestJSON = jsonutils:decode(wpart:fget("post:request")),
    Msg = jsonutils:get_attribute(RequestJSON, <<"msg">>),
    {content, text, e_socat:command(Msg)}.


