-module(averell).

-define(info(Msg), error_logger:info_msg(Msg)).
-define(info(Msg, Data), error_logger:info_msg(Msg, Data)).

-define(error(Msg), error_logger:error_msg(Msg)).
-define(error(Msg, Data), error_logger:error_msg(Msg, Data)).

-define(port, 8000).

-export([main/1]).

main([]) ->
    case file:get_cwd() of
	{ok, Dir} ->
	    start(Dir, ?port);
	{error, Reason} ->
	    ?error("Unable to get cwd: ~p~n", [Reason]),
	    halt(1)
    end;
main([Dir]) ->
    case filelib:is_dir(Dir) of
	true ->
	    start(Dir, ?port);
	false ->
	    ?error("Not a dir: ~p~n", [Dir]),
	    halt(1)
    end;
main(_) ->
    usage(),
    halt(1).

%%%
%%% Priv
%%%
usage() ->
    io:format("Usage: averell [dir]~n").

start(Dir, Port) ->
    Handler = {"/[...]", cowboy_static, 
	       {dir, Dir, [{mimetypes, cow_mimetypes, all}]}},
    Dispatch = cowboy_router:compile([{'_', [Handler]}]),
    cowboy:start_http(http, 5, [{port, Port}], 
		      [{env, [{dispatch, Dispatch}]}]).
