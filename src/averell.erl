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

-include("averell.hrl").

-define(port, 8000).

-define(argspec, [
		  {help,    $h,        "help",    undefined,           "Show help"},
		  {port,    $p,        "port",    {integer, ?port},    "Port number"},
		  {cors,    $c,        "cors",    {boolean, false},    "Enable CORS (allowed origins: *)"},
		  {access,  $a,        "access",  {boolean, false},    "Use .avlaccess files"},
		  {verbose, $v,        "verbose", {boolean, false},    "Verbose"},
		  {debug,   $d,        "debug",   {boolean, false},    "Debug (more verbose)"},
		  {noindex, $I,        "no-index",{boolean, false},    "Do not server index.html automatically"},
		  {dir,     undefined, undefined, {string, get_cwd()}, "Directory to serve"}
		 ]).

-export([main/1]).

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
    averell_sup:start_link(Opts),
    loop().

loop() ->
    receive
	_ ->
	    loop()
    end.
