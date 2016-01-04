%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2015, Jean Parpaillon
%%% @doc
%%%
%%% @end
%%% Created :  17 Dec 2015 by Jean Parpaillon <jean.parpaillon@free.fr>

-module(averell_SUITE).

-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PORT, 9999).
-define(ENDPOINT, "http://localhost:9999").

groups() ->
    [
     {all, [], [
		test_index
	       ,test_explicit
	       ,test_mimetype
	       ,test_access_index
	       ]},
     {all_default, [], [{group, all}]}
    ].


all() ->
	[
	 {group, all_default}
	].


init_per_suite(Config) ->
    application:start(inets),
    Config.

end_per_suite(_Config) ->
    ok.


init_per_group(_, Config) ->
    Pid = start(Config, []),
    timer:sleep(2000),
    [{pid, Pid} | Config].

end_per_group(_, Config) ->    
    exit(?config(pid, Config), exit),
    timer:sleep(2000),
    ok.


test_index(_Config) ->
    {ok, {{_, Code, _}, _Headers, Body}} = httpc:request(?ENDPOINT ++ "/"),
    ?assertEqual(200, Code),
    ?assertEqual("<html>\n"
		 "  <head>\n"
		 "    <title>test</title>\n"
		 "  </head>\n"
		 "  <body>\n"
		 "    <p>test</p>\n"
		 "  </body>\n"
		 "</html>\n"
		, Body).


test_explicit(_Config) ->
    {ok, {{_, Code, _}, _Headers, Body}} = httpc:request(?ENDPOINT ++ "/index.html"),
    ?assertEqual(200, Code),
    ?assertEqual("<html>\n"
		 "  <head>\n"
		 "    <title>test</title>\n"
		 "  </head>\n"
		 "  <body>\n"
		 "    <p>test</p>\n"
		 "  </body>\n"
		 "</html>\n"
		, Body).    


test_mimetype(_Config) ->
    {ok, {{_, Code, _}, Headers, _Body}} = httpc:request(?ENDPOINT ++ "/erlang-logo.png"),
    ?assertEqual(200, Code),
    CT = lists:keysearch("content-type", 1, Headers),
    ?assertEqual({value, {"content-type", "image/png"}}, CT).


test_access_index(_Config) ->
    {ok, {{_, Code, _}, _Headers, Body}} = httpc:request(?ENDPOINT ++ "/with-access/"),
    ?assertEqual(200, Code),
    ?assertEqual("default\n", Body).

%%%
%%% Priv
%%%
start(Config, Opts) ->
    Exe = filename:join([?config(data_dir, Config), "averell"]),
    Dir = filename:join([?config(data_dir, Config), "www"]),
    Cmd = Exe ++ " -p " ++ integer_to_list(?PORT) ++ " " ++ Opts ++ " " ++ Dir,
    ct:log(default, ?STD_IMPORTANCE, "CMD: ~s", [Cmd]),    
    spawn(fun() -> os:cmd(Cmd) end).
