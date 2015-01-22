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
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init(Opts) ->
    C = case proplists:get_bool(access, Opts) of
	    true -> [get_access_config(Opts)];
	    false -> []
	end,
    C2 = [ get_http_config(Opts) | C ],
    {ok, {{one_for_one, 10, 10}, C2}}.

get_access_config(Opts) ->
    {averell_access, {averell_access, start_link, [Opts]}, permanent, 5000, worker, []}.


get_http_config(Opts) ->
    Dir = proplists:get_value(dir, Opts),
    Port = proplists:get_value(port, Opts),
    Index = case proplists:get_bool(noindex, Opts) of
		true -> {index, false};
		false -> {index, true}
	    end,
    Dispatch = cowboy_router:compile([
				      {'_', 
				       [
					{"/[...]", averell_handler, {Dir, [ Index ]}}]
				      }
				     ]),
    ProtoOpts = case proplists:get_bool(debug, Opts) of
		    true -> 
			[ {onresponse, fun averell_handler:onresponse2/4},
			  {onrequest, fun averell_handler:onrequest2/1} ];
		    false -> 
			case proplists:get_bool(verbose, Opts) of
			    true ->
				[  {onresponse, fun averell_handler:onresponse1/4},
				   {onrequest, fun averell_handler:onrequest1/1} ];
			    false ->
				[]
			end
		end,
    Env = lists_clean([
		       {dispatch, Dispatch},
		       case proplists:get_bool(cors, Opts) of
			   true -> {cors_policy, averell_cors};
			   false -> undefined
		       end
		      ]),
    Mw = case proplists:get_bool(cors, Opts) of
	     true -> [cowboy_router, cowboy_cors, cowboy_handler];
	     false -> [cowboy_router, cowboy_handler]
	 end,
    Args = [ http, 10, [{port, Port}], [ {env, Env}, {middlewares, Mw} | ProtoOpts ] ],
    ?info("Serving ~s on port ~p~n", [Dir, Port]),
    {averell_http, {cowboy, start_http, Args}, permanent, 5000, worker, [cowboy]}.

lists_clean(L) ->
    lists:filter(fun (E) -> E =/= undefined end, L).
