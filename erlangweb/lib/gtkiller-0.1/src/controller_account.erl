-module(controller_account).
-behaviour(controller).

-export([
    handle/1,
    select/0, select/1,
    create/0, create/1,
    update/0, update/1,
    delete/0, delete/1,
	delete_current/0, delete_current/1,
	read_xmpp_resources/0, read_xmpp_resources/1,
    search/0, search/1,
    set_custom_status/0
]).

-include("constants.hrl").
-include("utils_controller_annotations.hrl").


%%%
%%% Controller actions.
%%%

%%
handle(_Argument) ->
    controller:handle().

%%
select() ->
    controller:select().

select(error) ->
    controller:select(error).

%%
?PREPARE_DATA({account}).
?VALIDATE_DATA({}).
create() ->
    console:log(["Action executed:", {?MODULE, create}], 'DEBUG'),
    controller:create().

create(error) ->
    console:log(["Action failed:", {?MODULE, create}], 'ERROR'),
    controller:create(error).

%%
?CHECK_EXISTENCE({account, update}).
?PREPARE_DATA({account}).
?VALIDATE_DATA({}).
update() ->
    console:log(["Action executed:", {?MODULE, update}], 'DEBUG'),
    controller:update().

update(error) ->
    console:log(["Action failed:", {?MODULE, update}], 'ERROR'),
    controller:update(error).

%%
?CHECK_EXISTENCE({account, delete}).
delete() ->
    console:log(["Action executed:", {?MODULE, delete}], 'DEBUG'),
    controller:delete().

delete(error) ->
    [].

%%
%% Deletes logged in user, excepts admin
%%

delete_current() ->
    console:log(["Action executed:", {?MODULE, delete_current}], 'DEBUG'),
	Id = wpart:fget("session:account_id"),
	if Id =:= 1 ->
		[{<<"errors">>, [{struct, [{<<"error">>, 1}, {<<"description">>, <<"Can't remove admin">>}]}]}];
	true ->
		wtype_account:delete(Id),
		% console:log(["Item to delete:", Id], 'DEBUG'),
		[{<<"data">>, {struct, [{<<"id">>, Id}]}}]
	end.

delete_current(error) ->
    [].


%%
%% Selects user xmpp resources list
%%

read_xmpp_resources() ->
	Jid = wpart:fget("session:currentjid"),
	List = ejabberd:user_resources(Jid),
	[{<<"data">>, format_xmpp_resources(List)}]
	.

read_xmpp_resources(error) ->
	[].

%%
%% Formatter
%% @todo Rewrite to geenric formatter
%%
format_xmpp_resources([]) ->
	[]
	;
format_xmpp_resources(List) ->
	JSONEncoded = lists:map(
		fun(Elem) ->
			{<<"name">>, list_to_binary(Elem)}
		end,
		List
	),
	[{struct, JSONEncoded}]
	.

search() ->
    SearchResult = utils:search_from_request_for(account),
    encode_search_result(SearchResult).

search(error) ->
    [].

encode_search_result(SearchResult) ->
    [{<<"data">>, utils:format(account, SearchResult, {exclude, [password, fingerprint]})}].

%%
%% Handles offline custom status
%%

?AUTHORIZE(json).
set_custom_status() ->
    Status = jsonutils:get_attribute(wpart:fget(?KEY_COMMAND), <<"status">>),
    wtype_account:set_custom_status(Status),
    [{<<"status">>, list_to_binary(Status)}].