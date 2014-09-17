%%%-------------------------------------------------------------------
%%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%%% @copyright (C) 2014, Jean Parpaillon
%%% 
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%% 
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%% 
%%% @doc
%%%
%%% @end
%%% Created : 10 Sep 2014 by Jean Parpaillon <jean.parpaillon@free.fr>
%%%-------------------------------------------------------------------
-module(averell).

-define(info(Msg), io:format("INFO: " ++ Msg)).
-define(info(Msg, Data), io:format("INFO: " ++ Msg, Data)).

-define(error(Msg), io:format("ERROR: " ++ Msg)).
-define(error(Msg, Data), io:format("ERROR: " ++ Msg, Data)).

-define(port, 8000).

-define(argspec, [
		  {help,    $h,        "help",    undefined,           "Show help"},
		  {port,    $p,        "port",    {integer, ?port},    "Port number"},
		  {verbose, $v,        undefined, undefined,           "Verbose"},
		  {dir,     undefined, undefined, {string, get_cwd()}, "Directory to serve"}
		 ]).

-export([main/1, 
	 onresponse/4]).

main(Args) ->
    case getopt:parse(?argspec, Args) of
	{ok, {Opts, []}} ->
	    case proplists:get_bool(help, Opts) of
		true ->
		    getopt:usage(?argspec, atom_to_list(?MODULE)),
		    halt(0);
		false ->
		    start(Opts)
	    end;
	_ ->
	    getopt:usage(?argspec, atom_to_list(?MODULE)),
	    halt(1)
    end.

%%%
%%% Priv
%%%
get_cwd() ->
    case file:get_cwd() of
	{ok, Dir} ->
	    Dir;
	{error, Reason} ->
	    ?error("Unable to get cwd: ~p~n", [Reason]),
	    halt(1)
    end.

start(Opts) ->
    application:start(ranch),
    application:start(crypto),
    application:start(cowlib),
    application:start(cowboy),
    Dir = proplists:get_value(dir, Opts),
    Port = proplists:get_value(port, Opts),
    Handler = {"/[...]", cowboy_static, 
	       {dir, Dir, [{mimetypes, cow_mimetypes, all}]}},
    Dispatch = cowboy_router:compile([{'_', [Handler]}]),
    CowboyOpts = case proplists:get_bool(verbose, Opts) of
		     true ->
			 [{onresponse, fun ?MODULE:onresponse/4}];
		     false ->
			 []
		 end,
    case cowboy:start_http(http, 5, [{port, Port}], 
			   [{env, [{dispatch, Dispatch}]} | CowboyOpts]) of
	{ok, _} ->
	    ?info("Serving ~p on 0.0.0.0:~p~n", [Dir, proplists:get_value(port, Opts)]),
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

onresponse(Status, _Headers, _Body, Req) ->
    {Method, _} = cowboy_req:method(Req),
    {Path, _} = cowboy_req:path(Req),
    ?info("~s ~s - ~p~n", [Method, Path, Status]),
    Req.