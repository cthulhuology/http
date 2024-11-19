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

-module(sock).
-author({ "David J Goehrig", "dave@dloh.org"}).
-copyright(<<"© 2013,2023 David J. Goehrig"/utf8>>).
-export([ connect/3, send/2, data/2 ]).
-record(sock, { socket, host, port, ssl }).

loop(Sock = #sock{ socket = Socket, host = Host, port = Port, ssl = true }) ->
	receive 
	{ send, Data } ->
		ssl:send(Socket,Data);
	{ callback, Fun } ->
		put(callback,Fun);
	{ ssl, Socket, Data } ->
		F = get(callback),
		F(Data);
	{ ssl_closed, Socket } ->
		error_logger:info_msg("ssl ~p closed connection to ~p:~p~n", [ Socket, Host, Port ]);
	{ ssl_error, Socket, Reason } ->
		error_logger:info_msg("ssl ~p error ~p~n", [ Socket, Reason ]);
	{ ssl_passive, Socket } ->
		error_logger:info_msg("ssl ~p now passive~n", [ Socket ])
	end,
	loop(Sock);

loop(Sock = #sock{ socket = Socket, host = Host, port = Port, ssl = false }) ->
	receive 
	{ send, Data } ->
		gen_tcp:send(Socket,Data);
	{ callback, Fun } ->
		put(callback,Fun);
	{ tcp, Socket, Data } ->
		F = get(callback),
		F(Data);
	{ tcp_passive, Socket } ->
		error_logger:info_msg("tcp ~p now passive~n", [ Socket ]);
	{ tcp_closed, Socket } ->
		error_logger:info_msg("tcp ~p closed connection to ~p:~p~n", [ Socket, Host, Port ]);
	{ tcp_error, Socket, Reason } ->
		error_logger:info_msg("tcp ~p error ~p~n", [ Socket, Reason ])
	end,
	loop(Sock).

connect(ssl,Host,Port) ->
	spawn(fun () ->
		case ssl:connect(Host,Port,[{verify,verify_peer}, {cacerts, public_key:cacerts_get() }, binary]) of
			{ ok, Socket } ->
				ok = ssl:controlling_process(Socket,self()),
				loop(#sock{ socket = Socket, host = Host, port = Port, ssl = true });
			{ error, Reason } ->
				error_logger:error_msg("Failed to connect to ~s:~p, ~p~n", [ Host, Port, Reason ])
		end
	end);

connect(tcp,Host,Port) ->
	spawn(fun () ->
		case gen_tcp:connect(Host,Port, [ {active,true}, binary ]) of 
			{ ok, Socket } ->
				ok = gen_tcp:controlling_process(Socket,self()),
				loop(#sock{ socket = Socket, host = Host, port = Port, ssl = false });
			{ error, Reason } ->
				error_logger:error_msg("Failed to connect to ~s:~p, ~p~n", [ Host, Port, Reason ])
		end
	end).

send(Pid,Data) ->
	Pid ! { send, Data }.

data(Pid,Fun) ->
	Pid ! { callback, Fun }.
