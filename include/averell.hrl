-define(debug(Msg), io:format("DEBUG: " ++ Msg ++ "~n", [])).
-define(debug(Msg, Data), io:format("DEBUG: " ++ Msg ++ "~n", Data)).

-define(info(Msg), io:format(Msg ++ "~n", [])).
-define(info(Msg, Data), io:format(Msg ++ "~n", Data)).

-define(error(Msg), io:format("ERROR: " ++ Msg ++ "~n", [])).
-define(error(Msg, Data), io:format("ERROR: " ++ Msg ++ "~n", Data)).

-type auth_method() :: basic | digest.

-type auth_opt() :: {auth_user_file, binary()} 
		  | {auth_group_file, binary()}
		  | {auth_name, binary()}
		  | {auth_rule, {user, [binary()]} | {group, [binary()]} | 'valid-user'}
		  | {auth_method, auth_method()}.

-type index_opt() :: {index, noindex | binary()}.

-type avl_opts() :: auth_opt() | index_opt().

-type avlaccess() :: [avl_opts()].
