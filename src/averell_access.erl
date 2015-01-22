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

%% @doc avleccess store

-module(averell_access).
-author('Jean Parpaillon <jean.parpaillon@free.fr>').

-include("averell.hrl").

-behaviour(gen_server).

-export([start_link/1,
	 get_info/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(ACCESSPATH, '.avlaccess').
-define(SERVER, ?MODULE).
-define(TNAME, ?MODULE).
-record(state, {tid       :: ets:tid(),
		basedir   :: binary()}).

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

-spec get_info(Path :: file:name_all()) -> avlaccess().
get_info(Path) ->
    case whereis(?SERVER) of
	undefined -> [];
	_ -> gen_server:call(?SERVER, {info, Path})
    end.

%%%
%%% gen_server callbacks
%%%

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init(list()) -> {ok, term()} | {error, term()} | ignore.
init(Opts) ->
    Dir = proplists:get_value(dir, Opts),
    Tid = ets:new(?TNAME, [named_table,
			   {read_concurrency, true},
			   protected]),
    {ok, #state{tid=Tid, basedir=Dir}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({info, Path}, _From, #state{tid=Tid, basedir=Basedir}=State) ->
    {reply, get_info2(Basedir, Path, Tid), State};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%% Private
%%%
get_info2(Basedir, Path, Tid) ->
    case ets:lookup(Tid, Path) of
	[] ->
	    Info = load_info(Basedir, Path, Tid),
	    Merged = case Path of 
			 <<".">> -> Info;
			 _ -> merge_info(get_info2(Basedir, filename:dirname(Path), Tid), Info)
		     end,
	    ets:insert(Tid, {Path, Merged}),
	    Merged;
	[ {_, Info} ]-> Info
    end.

load_info(Basedir, Path, _Tid) ->
    AccessPath = filename:join([Basedir, Path, ".avlaccess"]),
    case file:consult(AccessPath) of
	{ok, [Infos]} -> Infos;
	{ok, _} -> ?error("Syntax error in ~p", [AccessPath]), [];
	{error, enoent} -> [];
	{error, Err} -> ?error("Error reading ~p: ~p", [AccessPath, Err]), []
    end.

merge_info(ParentInfo, []) ->
    ParentInfo;
merge_info(ParentInfo, [ {Key, Val} | Tail ]) ->
    merge_info(lists:keystore(Key, 1, ParentInfo, {Key, Val}), Tail).

