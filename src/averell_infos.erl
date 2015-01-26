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

%% @doc Get options for served file, from options or .avlaccess files

-module(averell_infos).
-author('Jean Parpaillon <jean.parpaillon@free.fr>').

-include("averell.hrl").

-export([start/0,
	 get_info/1]).

-record(infos_table, {path         :: binary(),
		      infos         :: avlinfos()}).

-define(ACCESSPATH, '.avlaccess').
-define(TID, ?MODULE).

start() ->
    {atomic, ok} = mnesia:create_table(infos_table, [
						     {attributes, record_info(fields, infos_table)},
						     {ram_copies, nodes()},
						     {storage_properties, 
						      [{ets, [{read_concurrency, true}]}]
						     }
						    ]).


-spec get_info(Path :: file:name_all()) -> avlinfos().
get_info(Path) ->
    get_info(application:get_env(averell, dir, undefined), Path).


%%%
%%% Private
%%%
get_global() ->
    [
     {index, application:get_env(averell, index, <<"index.html">>)}
    ].

get_info(Basedir, <<".">>=Path) ->
    case mnesia:dirty_read(infos_table, Path) of
	[] ->
	    Info = merge_info(get_global(), load_info(Basedir, Path)),
	    mnesia:transaction(fun() -> mnesia:write(#infos_table{path=Path, infos=Info}) end),
	    Info;
	[ #infos_table{infos=Info} ]-> Info
    end;    
get_info(Basedir, Path) ->
    case mnesia:dirty_read(infos_table, Path) of
	[] ->
	    Info = merge_info(get_info(Basedir, filename:dirname(Path)), load_info(Basedir, Path)),
	    mnesia:transaction(fun() -> mnesia:write(#infos_table{path=Path, infos=Info}) end),
	    Info;
	[ #infos_table{infos=Info} ]-> Info
    end.

load_info(Basedir, Path) ->
    AccessPath = filename:join([Basedir, Path, ".avlaccess"]),
    case file:consult(AccessPath) of
	{ok, [Infos]} -> Infos;
	{ok, _} -> ?error("Syntax error in ~p", [AccessPath]), [];
	{error, enoent} -> [];
	{error, Err} -> ?error("Error reading ~p: ~p", [AccessPath, Err]), []
    end.

% Only accept defined set of values
merge_info(ParentInfo, []) ->
    ParentInfo;
merge_info(ParentInfo, [ {index, Val} | Tail ]) ->
    merge_info(lists:keystore(index, 1, ParentInfo, {index, Val}), Tail);
merge_info(ParentInfo, [ {Key, _} | Tail ]) ->
    ?debug("Ignore info: ~p", [Key]),
    merge_info(ParentInfo, Tail).
