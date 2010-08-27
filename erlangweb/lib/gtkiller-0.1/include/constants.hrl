
-define(ERROR_DATABASE_FAILED,        	'100').
-define(ERROR_COMMAND_UNDEFINED,      	'101').
-define(ERROR_COMMAND_UNKNOWN,        	'102').
-define(ERROR_USER_NOT_FOUND,         	'103').
-define(ERROR_USER_INACTIVE,          	'104').
-define(ERROR_DATA_NOT_VALID,           '105').
-define(ERROR_AUTH_REQUIRED,            '106').
-define(ERROR_BAD_COMMAND_STRUCTURE,    '107').
-define(ERROR_DUPLICATE_PRIMARY_KEY,    '108').
-define(ERROR_RECORD_LOST,              '109').
-define(ERROR_FORBIDDEN,                '110').

-define(KEY_COMMAND,  "_command").
-define(KEY_ID,       "_id").
-define(KEY_ITEM,     "_item").
-define(KEY_ERRORS,   "_errors").
-define(KEY_REQUEST,  "_request").
-define(KEY_RESPONSE, "_response").

-define(TO_JSON_NAME(Name), list_to_binary(atom_to_list(Name))).
-define(JSON_CODE,        <<"code">>).
-define(JSON_COMMANDS,    <<"commands">>).
-define(JSON_DATA,        <<"data">>).
-define(JSON_DESCRIPTION, <<"description">>).
-define(JSON_ERRORS,      <<"errors">>).
-define(JSON_ID,          <<"id">>).
-define(JSON_TYPE,        <<"type">>).
