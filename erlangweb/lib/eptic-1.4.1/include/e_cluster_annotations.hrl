-compile({parse_transform, e_user_annotation}).
-compile(nowarn_shadow_vars).

-define(INVALIDATE(Args), -ew_user_annotation({Args, 'after', e_cluster, invalidate})).

-define(INVALIDATE_IF(Args), -ew_user_annotation({Args, 'after', e_cluster, invalidate_if})).

-define(INVALIDATE_GROUPS(Args), -ew_user_annotation({Args, 'after', e_cluster, invalidate_groups})).

-define(INVALIDATE_GROUPS_IF(Args), -ew_user_annotation({Args, 'after', e_cluster, invalidate_groups_if})).

-define(BACKEND_CALL(Args), -ew_user_annotation({Args, before, e_cluster, backend_call})).

