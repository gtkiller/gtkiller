-module(wpart_flag).
-behaviour(wpart).

-export([handle_call/1, build_html_tag/4, build_html_tag/3, load_tpl/0]).
-deprecated([build_html_tag/4]).

-include_lib("xmerl/include/xmerl.hrl").


%%
handle_call(#xmlElement{attributes = Attrs0}) ->
    Attrs = wpart:xml2proplist(Attrs0),
    #xmlText{value = get_html_tag(Attrs, wpart:getValue(Attrs)), type = cdata}.

%%
build_html_tag(Id, Params, Default) ->
    Attrs0 = wpart:normalize_html_attrs(proplists:get_value(html_attrs, Params, [])),
    Attrs = [{"name", Id}, {"id", Id} | proplists:delete("name", Attrs0)],
    get_html_tag(Attrs, Default).

%%
build_html_tag(Name, Prefix, Params, Default) ->
    Description = wpart_derived:get_description(Name, Params),
    N = wpart_derived:generate_long_name(Prefix, Name),
    D = wpart_derived:find(N, Default),
    Attrs0 = wpart:normalize_html_attrs(proplists:get_value(html_attrs, Params, [])),
    Attrs = [{"name", N} | proplists:delete("name", Attrs0)],
    wpart_derived:surround_with_table(N, get_html_tag(Attrs, D), Description).

%%
get_html_tag(Attrs, Default) ->
    Checked = case Default of
        false ->
            [];
        _ ->
            [{"checked", "checked"}]
    end,
    wpart_gen:build_html(wpart_gen:tpl_get(flag), [{"html", wpart:proplist2html(Checked ++ Attrs)}]).

%%
load_tpl() ->
    wpart_gen:load_tpl(flag, filename:join([code:priv_dir(gtkiller), "html", "flag.tpl"])).
