-record(account, {
    id,
    name = "",
    password = "",
    email = "",
    active = true,
    jid = "",
    fingerprint = "",
    status = ""
}).

-record(account_types, {
    id = {integer, [
        primary_key,
        {private, true}
    ]},
    name = {string, [
        {description, "Name"},
        {max_length, 256},
        {min_length, 1}
    ]},
    password = {string, [
        {description, "Password"},
        {max_length, 20}
    ]},
    email = {string, [
        {description, "E-mail"},
        unique,
        {regexp, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}"},
        {max_length, 50}
    ]},
    active = {flag, [
        {description, "Is account active?"},
        {optional, true}
    ]},
    % hiden field based on email (replace "@" on "-at-")
    jid = {string, [
        {description, "Ejabberd User Name"},
        {private, true}
    ]},
    fingerprint = {string, [
        {description, "Fingerprint"},
        {private, true}
    ]},
    status = {string, [
        {description, "Custom status"},
        {max_length, 256},
        {min_length, 0},
	{optional, "test"}
    ]}
}).
