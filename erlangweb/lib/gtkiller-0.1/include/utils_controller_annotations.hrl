-compile({parse_transform, e_user_annotation}).
-compile(nowarn_shadow_vars).

-define(PROCESS_REQUEST(Args), -ew_user_annotation({Args, before, utils_controller, process_request})).

-define(PROCESS_RESPONSE(Args), -ew_user_annotation({Args, 'after', utils_controller, process_response})).

-define(PREPARE_DATA(Args), -ew_user_annotation({Args, before, utils_controller, prepare_data})).

-define(VALIDATE_DATA(Args), -ew_user_annotation({Args, before, utils_controller, validate_data})).

-define(CHECK_EXISTENCE(Args), -ew_user_annotation({Args, before, utils_controller, check_existence})).

-define(AUTHORIZE(Args), -ew_user_annotation({Args, before, utils_controller, authorize})).

