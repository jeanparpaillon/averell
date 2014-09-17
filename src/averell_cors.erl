-module(averell_cors).
-behaviour(cowboy_cors_policy).

-export([policy_init/1]).
-export([allowed_origins/2]).
-export([allowed_methods/2]).

policy_init(Req) ->
    {ok, Req, undefined}.

allowed_origins(Req, State) ->
    {'*', Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.
