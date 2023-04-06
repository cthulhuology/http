%%
%% MIT No Attribution  
%% Copyright 2023 David J Goehrig <dave@dloh.org>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy 
%% of this software and associated documentation files (the "Software"), to 
%% deal in the Software without restriction, including without limitation the 
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
%% sell copies of the Software, and to permit persons to whom the Software is 
%% furnished to do so.  
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
%% IN THE SOFTWARE.

-module(http).
-author({ "David J Goehrig", "dave@dloh.org"}).
-copyright(<<"© 2023 David J. Goehrig"/utf8>>).
-compile({no_auto_import,[get/1,put/1]}).
-export([ start/0, start/1, get/1, get/2, post/2, post/3, put/2, put/3, delete/1, delete/2, head/1, connect/2, options/2, trace/2, patch/3, creds/0, request/4, pid/0, then/1, body/1, body/2 ]).

pid() ->
	?MODULE ! { pid, self() }.

creds() ->
	?MODULE ! creds.	

then(Fun) ->
	?MODULE ! { then, Fun }.

get(Url) ->
	?MODULE ! { get, Url, [] }.

get(Url,Headers) ->
	?MODULE ! { get, Url, Headers }.

post(Url,Body) ->
	?MODULE ! { post, Url, [], Body }.

post(Url,Headers,Body) ->
	?MODULE ! { post, Url, Headers, Body }.

put(Url,Body) ->
	?MODULE ! { put, Url, [], Body }.

put(Url,Headers,Body) ->
	?MODULE ! { put, Url, Headers, Body }.

delete(Url) ->
	?MODULE ! { get, Url, [] }.

delete(Url,Headers) ->
	?MODULE ! { get, Url, Headers }.

head(Url) ->
	?MODULE ! { head, Url }.

connect(Url,Headers) ->
	?MODULE ! { connect, Url, Headers }.

options(Url,Headers) ->
	?MODULE ! { options, Url, Headers }.

trace(Url,Headers) ->
	?MODULE ! { trace, Url, Headers }.

patch(Url,Headers,Body) ->
	?MODULE ! { patch, Url, Headers, Body }.

binary_of(K) when is_atom(K) ->
	atom_to_binary(K);
binary_of(K) when is_list(K) ->
	list_to_binary(K);
binary_of(V) when is_binary(V) ->
	V;
binary_of(_) ->	%% everything else is empty string
	<<"">>.

%% headers returns a io_vec with K: V lines, NB: you if K supplied process dict referenced
headers([ K| T], Acc) when is_atom(K) ->
	headers([ { atom_to_binary(K), erlang:get(K) } | T ], Acc);
headers([{K,V}|T],Acc) when is_atom(K) ->
	headers([{atom_to_binary(K),V}|T],Acc);
headers([{K,V}|T],Acc) when is_list(K) ->
	headers([{list_to_binary(K),V}|T],Acc);
headers([{K,V}|T],Acc) when is_binary(K) ->
	Value = binary_of(V),
	headers(T, <<Acc/binary,K/binary,$:,32,Value/binary,13,10>>);
headers([],Acc) ->
	<<Acc/binary,13,10>>.	%% is an io_vec, empty line to end
headers(L) ->
	headers(L,<<"">>).

uppercase_of(M) when is_atom(M) ->
	uppercase_of(atom_to_list(M));
uppercase_of(M) when is_binary(M) ->
	uppercase_of(binary_to_list(M));
uppercase_of(M) when is_list(M) ->
	list_to_binary(string:uppercase(M)).

method(Method,Path) ->
	M = uppercase_of(Method),
	P = binary_of(Path),
	<<M/binary,32,P/binary,32,"HTTP/1.1",10>>.

request(Method,Url,Headers,Body) ->
	U = url:parse(Url),
	M = method(Method,url:path(U)),
	H = headers([ {'Host', url:host(U) } | Headers]),
	R = <<M/binary,H/binary,Body/binary>>,
	Socket = case url:protocol(U) of 
		https ->
			sock:connect(ssl,url:host(U),url:port(U));
		wss ->
			sock:connect(ssl,url:host(U),url:port(U));
		_ ->
			sock:connect(tcp,url:host(U),url:port(U))
	end,
	Self = self(),
	sock:data(Socket, fun(D) -> Self ! { data, D } end),
	sock:send(Socket,R).


contains_blank_line(Data) when is_binary(Data) ->
	case binary:matches(Data, <<"\r\n\r\n">>) of
		[] -> case binary:matches(Data,<<"\n\n">>) of
			[] -> false;
			[ { Offset, 2} | _M ] -> Offset+2
		end;
		[{Offset,4}| _M ] -> Offset+4
	end.

%% match_eol(Data) when is_binary(Data) ->
%% 	case binary:matches(Data, <<"\r\n">>) of
%% 		[] -> case binary:matches(Data, <<"\n">>) of
%% 			[] -> false;
%% 			[ {Offset,1} | _M ] -> Offset + 1
%% 		end;
%% 		[{Offset,2}| _M] -> Offset + 2
%% 	end.

content_length(Headers) ->
	{ ok, M } = re:compile("Content-Length: (?<Length>[0-9]+)"),
	{ match, [Length] } = re:run(Headers,M,[{capture,all_names,binary}]),
	binary_to_integer(Length).

response(Seen,Data) ->
	Resp = <<Seen/binary,Data/binary>>,
	case contains_blank_line(Resp) of
		false -> { wait, Resp };
		EOH ->
			Headers = binary:part(Resp, 0, EOH),
			Body = binary:part(Resp, EOH, byte_size(Resp) - EOH),
			Length = content_length(Headers),
			case byte_size(Body) < Length of
				true -> { wait, Resp };
				false -> {done, Headers, Body }
			end
	end.
			

body(Bin,Len) ->
	<<13,10,13,10, Body:Len/binary>> = string:find(Bin,[13,10,13,10]),
	Body.
body(Bin) ->
	<<13,10,13,10, Body/binary>> = string:find(Bin,[13,10,13,10]),
	Body.

%% Main process loop
loop(Seen) ->
	receive 
	{ connect, Url, Headers } ->
		request(connect,Url,Headers,<<"">>),
		loop(<<"">>);
	{ head, Url } ->
		request(head,Url,[],<<"">>),
		loop(<<"">>);
	{ get, Url, Headers } ->
		request(get,Url,Headers,<<"">>),
		loop(<<"">>);
	{ delete, Url, Headers } ->
		request(delete,Url,Headers,<<"">>),
		loop(<<"">>);
	{ trace, Url, Headers } ->
		request(trace,Url,Headers,<<"">>),
		loop(<<"">>);
	{ options, Url, Headers } ->
		request(trace,Url,Headers,<<"">>),
		loop(<<"">>);
	{ patch, Url, Headers, Body } ->
		request(post,Url,Headers,Body),
		loop(<<"">>);
	{ post, Url, Headers, Body } ->
		request(post,Url,Headers,Body),
		loop(<<"">>);
	{ put, Url, Headers, Body } ->
		request(put,Url,Headers,Body),
		loop(<<"">>);
	{ then, Fun } ->
		erlang:put(callback,Fun),
		loop(Seen);
	{pid, Pid} ->
		Pid ! self(),
		loop(Seen);
	creds ->
		{ok, Creds} = file:consult("creds"),
		[ erlang:put(K,V) || { K, V } <- Creds ],
		loop(Seen);

	{data, Data} ->
		case response(Seen,Data) of
			{ wait, Resp } -> 
				loop(Resp);
			{ done, _Header, Body } -> 
				F = erlang:get(callback),
				F(Body),
				loop(<<"">>)
		end
	end.


%% sets the default headers, so we can be lazy
init([{K,V}|T]) when is_binary(K) ->
	erlang:put(binary_to_atom(K),V),
	init(T);
init([{K,V}|T]) when is_list(K) ->
	erlang:put(list_to_atom(K),V),
	init(T);
init([{K,V}|T]) when is_atom(K) ->
	erlang:put(K,V),
	init(T);
init([]) ->
	ssl:start(),
	register(?MODULE,self()),
	loop(<<"">>).

start(L) when is_list(L) ->
	spawn_link(fun() -> init(L) end).

start() ->
	start([]).
