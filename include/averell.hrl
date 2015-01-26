-define(LOG_NO,    -2).
-define(LOG_ERROR, -1).
-define(LOG_INFO,   0).
-define(LOG_DEBUG,  1).
-define(LOG_TRACE,  2).

-define(debug(Msg),       averell_log:log(?LOG_DEBUG, "DEBUG: ", Msg, [])).
-define(debug(Msg, Data), averell_log:log(?LOG_DEBUG, "DEBUG: ", Msg, Data)).

-define(info(Msg),        averell_log:log(?LOG_INFO, "INFO: ", Msg, [])).
-define(info(Msg, Data),  averell_log:log(?LOG_INFO, "INFO: ", Msg, Data)).

-define(error(Msg),       averell_log:log(?LOG_ERROR, "ERROR: ", Msg, [])).
-define(error(Msg, Data), averell_log:log(?LOG_ERROR, "ERROR: ", Msg, Data)).

-type auth_method() :: basic | digest.

-type auth_opt() :: {auth_user_file, binary()} 
		  | {auth_group_file, binary()}
		  | {auth_name, binary()}
		  | {auth_rule, {user, [binary()]} | {group, [binary()]} | 'valid-user'}
		  | {auth_method, auth_method()}.

-type index_opt() :: {index, noindex | binary()}.

-type avl_opt() :: auth_opt() | index_opt().

-type avlinfos() :: [avl_opt()].
