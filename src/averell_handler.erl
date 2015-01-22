%% Copyright (c) 2014, Jean Parpaillon <jean.parpaillon@free.fr>
%%
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
%% @doc averell_static is largely derived from cowboy_static from cowboy app,
%% written by Loïc Hoguin <essen@ninenines.eu>
%%
-module(averell_handler).

-include("averell.hrl").

-export([init/3,
	 rest_init/2,
	 malformed_request/2,
	 forbidden/2,
	 content_types_provided/2,
	 resource_exists/2,
	 last_modified/2,
	 generate_etag/2,
	 get_file/2]).

-export([onrequest1/1,
	 onrequest2/1,
	 onresponse1/4,
	 onresponse2/4]).

-type extra_index() :: {index, boolean()}.
-type extra_etag() :: {etag, module(), function()} | {etag, false}.
-type extra_mimetypes() :: {mimetypes, module(), function()}
			 | {mimetypes, binary() | {binary(), binary(), [{binary(), binary()}]}}.
-type extra() :: [extra_etag() | extra_mimetypes() | extra_index()].
-type opts() :: {string() | binary(), extra()}.
-export_type([opts/0]).

-include_lib("kernel/include/file.hrl").

-type state() :: {binary(), {ok, #file_info{}} | {error, atom()}, avlaccess(), extra()}.

%
% Verbose logging
%
onrequest1(Req) ->
    Req.

onresponse1(Status, _Headers, _Body, Req) ->
    Method = cowboy_req:get(method, Req),
    Path = cowboy_req:get(path, Req),
    ?info("~s ~s - ~p", [Method, Path, Status]),
    Req.

%
% Debug logging
%
onrequest2(Req) ->
    ?info("### REQUEST HEADERS"),
    {Method, _} = cowboy_req:method(Req),
    {Path, _} = cowboy_req:path(Req),
    {Version, _} = cowboy_req:version(Req),
    {Hdrs, _} = cowboy_req:headers(Req),
    ?info("~s ~s ~s", [Method, Path, Version]),
    lists:map(fun ({K, V}) ->
		      ?info("\t~s: ~s", [K, V])
	      end, Hdrs),
    case cowboy_req:has_body(Req) of
	true ->
	    case cowboy_req:body_length(Req) of
		{undefined, _} -> ?info("### REQUEST BODY: empty ");
		{0, _} -> ?info("### REQUEST BODY: empty ");
		{Length, _} -> ?info("### REQUEST BODY: ~p", [Length])
	    end;
	false -> 
	    ok
    end,    
    Req.

onresponse2(Status, Headers, Body, Req) ->
    ?info("### RESPONSE HEADERS"),
    Version = cowboy_req:get(version, Req),
    ?info("~s ~p", [Version, Status]),
    lists:map(fun ({K, V}) ->
		      ?info("\t~s: ~s", [K, V])
	      end, Headers),
    case Body of
	<<>> -> ?info("### RESPONSE BODY: empty or streamed ");
	_ -> ?info("### RESPONSE BODY: < ~p bytes >", [byte_size(Body)])
    end,
    Req.

-spec init(_, _, _) -> {upgrade, protocol, cowboy_rest}.
init(_, _, _) ->
    {upgrade, protocol, cowboy_rest}.

%% Resolve the file that will be sent and get its file information.
%% If the handler is configured to manage a directory, check that the
%% requested file is inside the configured directory.

-spec rest_init(Req, opts())
	       -> {ok, Req, error | state()}
		      when Req::cowboy_req:req().
rest_init(Req, {Path, Extra}) when is_list(Path) ->
    rest_init(Req, {list_to_binary(Path), Extra});
rest_init(Req, {Path, Extra}) ->
    Dir = fullpath(filename:absname(Path)),
    {PathInfo, Req2} = case cowboy_req:path_info(Req) of
			   {[], R} -> {[], R};
			   {PI, R} -> {filename:join(PI), R}
		       end,
    Filepath = filename:join([Dir, PathInfo]),
    Len = byte_size(Dir),
    case fullpath(Filepath) of
	<< Dir:Len/binary >> ->
	    rest_init_info(Req2, Filepath, fullpath(PathInfo), Extra);
	<< Dir:Len/binary, $/, _/binary >> ->
	    rest_init_info(Req2, Filepath, fullpath(PathInfo), Extra);
	_ ->
	    {ok, Req2, error}
    end.


fullpath([]) ->
    "/";
fullpath(Path) ->
    fullpath(filename:split(Path), []).

fullpath([], Acc) ->
    filename:join(lists:reverse(Acc));
fullpath([<<".">>|Tail], Acc) ->
    fullpath(Tail, Acc);
fullpath([<<"..">>|Tail], Acc=[_]) ->
    fullpath(Tail, Acc);
fullpath([<<"..">>|Tail], [_|Acc]) ->
    fullpath(Tail, Acc);
fullpath([Segment|Tail], Acc) ->
    fullpath(Tail, [Segment|Acc]).


rest_init_info(Req, Path, Reqpath, Extra) ->
    case file:read_file_info(Path, [{time, universal}]) of
	{ok, #file_info{type=regular}=Info} ->
	    {ok, Req, {Path, {ok, Info}, averell_access:get_info(Reqpath), Extra}};
	{ok, #file_info{type=directory}=Info} ->
	    case lists:keyfind(index, 1, Extra) of
		false ->
		    rest_init_dir(Req, Path, Reqpath, Extra);
		{index, true} ->
		    rest_init_dir(Req, Path, Reqpath, Extra);
		{index, false} ->
		    {ok, Req, {Path, {ok, Info}, [], Extra}}
	    end;
	{error, Err} ->
	    {ok, Req, {Path, {error, Err}, [], Extra}}
    end.


rest_init_dir(Req, Path, Reqpath, Extra) ->
    IndexPath = filename:join([Path, <<"index.html">>]),
    Info = file:read_file_info(IndexPath, [{time, universal}]),
    {ok, Req, {IndexPath, Info, averell_access:get_info(Reqpath), Extra}}.


-ifdef(TEST).
fullpath_test_() ->
    Tests = [
	     {<<"/home/cowboy">>, <<"/home/cowboy">>},
	     {<<"/home/cowboy">>, <<"/home/cowboy/">>},
	     {<<"/home/cowboy">>, <<"/home/cowboy/./">>},
	     {<<"/home/cowboy">>, <<"/home/cowboy/./././././.">>},
	     {<<"/home/cowboy">>, <<"/home/cowboy/abc/..">>},
	     {<<"/home/cowboy">>, <<"/home/cowboy/abc/../">>},
	     {<<"/home/cowboy">>, <<"/home/cowboy/abc/./../.">>},
	     {<<"/">>, <<"/home/cowboy/../../../../../..">>},
	     {<<"/etc/passwd">>, <<"/home/cowboy/../../etc/passwd">>}
	    ],
    [{P, fun() -> R = fullpath(P) end} || {R, P} <- Tests].

good_path_check_test_() ->
    Tests = [
	     <<"/home/cowboy/file">>,
	     <<"/home/cowboy/file/">>,
	     <<"/home/cowboy/./file">>,
	     <<"/home/cowboy/././././././file">>,
	     <<"/home/cowboy/abc/../file">>,
	     <<"/home/cowboy/abc/../file">>,
	     <<"/home/cowboy/abc/./.././file">>
	    ],
    [{P, fun() ->
		 case fullpath(P) of
		     << "/home/cowboy/", _/binary >> -> ok
		 end
	 end} || P <- Tests].

bad_path_check_test_() ->
    Tests = [
	     <<"/home/cowboy/../../../../../../file">>,
	     <<"/home/cowboy/../../etc/passwd">>
	    ],
    [{P, fun() ->
		 error = case fullpath(P) of
			     << "/home/cowboy/", _/binary >> -> ok;
			     _ -> error
			 end
	 end} || P <- Tests].

good_path_win32_check_test_() ->
    Tests = case os:type() of
		{unix, _} ->
		    [];
		{win32, _} ->
		    [
		     <<"c:/home/cowboy/file">>,
		     <<"c:/home/cowboy/file/">>,
		     <<"c:/home/cowboy/./file">>,
		     <<"c:/home/cowboy/././././././file">>,
		     <<"c:/home/cowboy/abc/../file">>,
		     <<"c:/home/cowboy/abc/../file">>,
		     <<"c:/home/cowboy/abc/./.././file">>
		    ]
	    end,
    [{P, fun() ->
		 case fullpath(P) of
		     << "c:/home/cowboy/", _/binary >> -> ok
		 end
	 end} || P <- Tests].

bad_path_win32_check_test_() ->
    Tests = case os:type() of
		{unix, _} ->
		    [];
		{win32, _} ->
		    [
		     <<"c:/home/cowboy/../../secretfile.bat">>,
		     <<"c:/home/cowboy/c:/secretfile.bat">>,
		     <<"c:/home/cowboy/..\\..\\secretfile.bat">>,
		     <<"c:/home/cowboy/c:\\secretfile.bat">>
		    ]
	    end,
    [{P, fun() ->
		 error = case fullpath(P) of
			     << "c:/home/cowboy/", _/binary >> -> ok;
			     _ -> error
			 end
	 end} || P <- Tests].
-endif.

%% Reject requests that tried to access a file outside
%% the target directory.

-spec malformed_request(Req, State)
		       -> {boolean(), Req, State}.
malformed_request(Req, State) ->
    {State =:= error, Req, State}.

%% Directories, files that can't be accessed at all and
%% files with no read flag are forbidden.

-spec forbidden(Req, State)
	       -> {boolean(), Req, State}
		      when State::state().
forbidden(Req, State={_, {ok, #file_info{type=directory}}, _, _}) ->
    {true, Req, State};
forbidden(Req, State={_, {error, eacces}, _, _}) ->
    {true, Req, State};
forbidden(Req, State={_, {ok, #file_info{access=Access}}, _, _})
  when Access =:= write; Access =:= none ->
    {true, Req, State};
forbidden(Req, State) ->
    {false, Req, State}.

%% Detect the mimetype of the file.

-spec content_types_provided(Req, State)
			    -> {[{binary(), get_file}], Req, State}
				   when State::state().
content_types_provided(Req, State={Path, _, _, _}) ->
    {[{cow_mimetypes:web(Path), get_file}], Req, State}.

%% Assume the resource doesn't exist if it's not a regular file.

-spec resource_exists(Req, State) -> {boolean(), Req, State} when State::state().
resource_exists(Req, State={_, {ok, #file_info{type=regular}}, _, _}) ->
    {true, Req, State};
resource_exists(Req, State) ->
    {false, Req, State}.

%% Generate an etag for the file.

-spec generate_etag(Req, State)
		   -> {{strong | weak, binary()}, Req, State}
			  when State::state().
generate_etag(Req, State={Path, {ok, #file_info{size=Size, mtime=Mtime}}, _, Extra}) ->
    case lists:keyfind(etag, 1, Extra) of
	false ->
	    {generate_default_etag(Size, Mtime), Req, State};
	{etag, Module, Function} ->
	    {Module:Function(Path, Size, Mtime), Req, State};
	{etag, false} ->
	    {undefined, Req, State}
    end.

generate_default_etag(Size, Mtime) ->
    {strong, integer_to_binary(erlang:phash2({Size, Mtime}, 16#ffffffff))}.

%% Return the time of last modification of the file.

-spec last_modified(Req, State)
		   -> {calendar:datetime(), Req, State}
			  when State::state().
last_modified(Req, State={_, {ok, #file_info{mtime=Modified}}, _, _}) ->
    {Modified, Req, State}.

%% Stream the file.
%% @todo Export cowboy_req:resp_body_fun()?

-spec get_file(Req, State)
	      -> {{stream, non_neg_integer(), fun()}, Req, State}
		     when State::state().
get_file(Req, State={Path, {ok, #file_info{size=Size}}, _, _}) ->
    Sendfile = fun (Socket, Transport) ->
		       case Transport:sendfile(Socket, Path) of
			   {ok, _} -> ok;
			   {error, closed} -> ok;
			   {error, etimedout} -> ok
		       end
	       end,
    {{stream, Size, Sendfile}, Req, State}.
