%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2015 Jean Parpaillon.
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

%% @doc Logger for the averell application.

-module(averell_log).
-author('Jean Parpaillon <jean.parpaillon@free.fr>').

-include("averell.hrl").

%% External exports
-export([start_link/0,
	 debug/2,
	 info/2,
	 error/2,
	 log/4]).

%% Internals
-export([init/0]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the logger.
start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.

debug(Msg, Data) -> log(?LOG_DEBUG, "DEBUG: ", Msg, Data).
info(Msg, Data) ->  log(?LOG_INFO,  "INFO: ",  Msg, Data).
error(Msg, Data) -> log(?LOG_ERROR, "ERROR: ", Msg, Data).


log(Level, Prefix, Msg, Data) ->
    try ?MODULE ! {log, Level, Prefix, Msg, Data}
    catch error:badarg -> io:format(Msg ++ "~n", Data)
    end.

%% @spec init() -> ok.
%% @doc init log loop.
init() ->
    LogLevel = application:get_env(averell, log, ?LOG_INFO),
    loop(LogLevel).

loop(MinLevel) ->
    receive
	{log, Level, Prefix, Msg, Data} when Level >= MinLevel ->
	    io:format(Prefix ++ Msg ++ "~n", Data),
	    loop(MinLevel);
	{log, _, _, _, _} -> 
	    loop(MinLevel)
    end.
