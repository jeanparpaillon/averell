-module(averell).

-define(info(Msg), io:format("INFO: " ++ Msg)).
-define(info(Msg, Data), io:format("INFO: " ++ Msg, Data)).

-define(error(Msg), io:format("ERROR: " ++ Msg)).
-define(error(Msg, Data), io:format("ERROR: " ++ Msg, Data)).

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
	    ?error("Not a dir: ~s~n", [Dir]),
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
    application:start(ranch),
    application:start(crypto),
    application:start(cowlib),
    application:start(cowboy),
    Handler = {"/[...]", cowboy_static, 
	       {dir, Dir, [{mimetypes, cow_mimetypes, all}]}},
    Dispatch = cowboy_router:compile([{'_', [Handler]}]),
    case cowboy:start_http(http, 5, [{port, Port}], 
			   [{env, [{dispatch, Dispatch}]}]) of
	{ok, _} ->
	    ?info("Serving ~p on 0.0.0.0:~p~n", [Dir, Port]),
	    loop();
	{error, Err} ->
	    ?error("Error starting server: ~p~n", [Err]),
	    halt(1)
    end.

loop() ->
    receive
	_ ->
	    loop()
    end.

