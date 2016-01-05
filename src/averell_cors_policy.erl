%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @author Daniel White
%% @doc Cross-Origin Resource Sharing (CORS) policy behaviour.
%%
%% This code is imported from https://github.com/danielwhite/cowboy_cors.git
%%
%% @see http://www.w3.org/TR/cors/
-module(averell_cors_policy).

-type state() :: any().

-callback policy_init(Req)
        -> {ok, Req, state()}
        when Req :: cowboy_req:req().
