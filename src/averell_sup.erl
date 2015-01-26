%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @copyright 2014 Jean Parpaillon.
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

%% @doc Supervisor for the averell application.

-module(averell_sup).
-author('Jean Parpaillon <jean.parpaillon@free.fr>').

-include("averell.hrl").

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init(_) ->
    Children = [ get_http_child(), get_log_child() ],
    {ok, {{one_for_one, 10, 10}, Children}}.

get_log_child() ->
    {averell_log, {averell_log, start_link, []}, permanent, 5000, worker, []}.

get_http_child() ->
    Dir = application:get_env(averell, dir, undefined),
    Port = application:get_env(averell, port, 80),
    Index = application:get_env(averell, index, noindex),
    Dispatch = cowboy_router:compile([
				      {'_', 
				       [
					{"/[...]", averell_handler, Dir}]
				      }
				     ]),
    ProtoOpts = case application:get_env(averell, log, ?LOG_INFO) of
		    Lvl when Lvl >= ?LOG_DEBUG ->
			[ {onresponse, fun averell_handler:onresponse/4},
			  {onrequest, fun averell_handler:onrequest/1} ];
		    _ ->
			[]
		end,
    Env = lists_clean([
		       {dispatch, Dispatch},
		       case application:get_env(averell, cors, falsexs) of
			   true -> {cors_policy, averell_cors};
			   false -> undefined
		       end
		      ]),
    Mw = case application:get_env(averell, cors, false) of
	     true -> [cowboy_router, cowboy_cors, cowboy_handler];
	     false -> [cowboy_router, cowboy_handler]
	 end,
    Args = [ http, 10, [{port, Port}], [ {env, Env}, {middlewares, Mw} | ProtoOpts ] ],
    ?info("Serving ~s on port ~p~n", [Dir, Port]),
    {averell_http, {cowboy, start_http, Args}, permanent, 5000, worker, [cowboy]}.

lists_clean(L) ->
    lists:filter(fun (E) -> E =/= undefined end, L).
