-module(wtype_account).

-export([
    get_record_info/1,

    % CRUD
    create/1,
    read/1,
    update/1,
    delete/1,

    prepare_initial/0,
    prepare_validated/0,
    prepare_edit/1,

    % fromating for output
    format/1,
    format_json/1,

    check_credentials/2,

    search/2
]).

-include("account.hrl").

get_record_info(account) -> record_info(fields, account);
get_record_info(account_types) -> #account_types{}.

create(AccountArg) ->

    % prepopulate "jid" from "email"
    AccountWithJid = setelement(7, AccountArg, re:replace(element(5, AccountArg), "@", "-at-", [{return, list}])),

    % set next id if needed
    Account = db:set_object_id(account, AccountWithJid),
    console:log(["Creating account:", Account]),

    % add to accounts table
    e_db:write(account, Account),
    console:log('OK, account is created'),


    Account.


read(all) ->
    e_db:read(account);
read(Id) ->
    e_db:read(account, Id).

update(Account) ->
    console:log(["UPDATE:", Account]),
    e_db:update(account, Account),
    Account.

delete(Id) ->
    Account = read(Id),
    e_db:delete(account, Id).

check_credentials(Username, Password) ->
    case db:search(account, jid, Username) of
        [#account{password=UserPassword}] when UserPassword =:= Password -> true;
        _ -> false
    end.


%%
%% Handles offline custom status
%%


prepare_initial() ->
    wpart_db:build_record_structure(account, #account{}).

prepare_validated() ->
    Account = wpart:fget("__not_validated"),
    wpart_db:build_record_structure(account, Account).

prepare_edit(Item) ->
    wpart_db:build_record_structure(account, Item).

search(Attribute, Value) ->
    db:search(account, Attribute, Value).

format(Item) ->
    [{"id", Item#account.id},
     {"name", Item#account.name},
     {"password", Item#account.password},
     {"fingerprint", Item#account.fingerprint},
     {"jid", Item#account.jid},
     {"active", Item#account.active},
     {"email", Item#account.email}].

format_json(Item) ->
    utils:format(account, Item).
