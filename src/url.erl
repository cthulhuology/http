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


-module(url).
-author({ "David J Goehrig", "dave@dloh.org"}).
-copyright(<<"© 2013,2023 David J. Goehrig"/utf8>>).
-export([ parse/1, host/1, port/1, path/1, protocol/1 ]).

parse(Url) ->
	{ ok, M } = re:compile("(?<Proto>[^:]+)://(?<Host>[^:/]+)(:(?<Port>[^/]+))?(?<Path>/[^?]*)(?<Query>.*)"),
	{ namelist, NL } = re:inspect(M,namelist),
	case re:run(Url,M,[{capture,all_names,binary}]) of
		{ match, MT } ->
			lists:zip(NL,MT);
		nomatch ->
			error_logger:error_msg("Failed to parse URL ~p~n", [Url ]),
			[]
	end.

protocol(Url) ->
	binary_to_atom(proplists:get_value(<<"Proto">>,Url)).

host(Url) ->
	binary:bin_to_list(proplists:get_value(<<"Host">>,Url)).

port_number(http) -> 80;
port_number(ws) -> 80;
port_number(https) -> 443;
port_number(wss) -> 443.

port(Url) ->
	case proplists:get_value(<<"Port">>,Url) of
		<<>> -> port_number(protocol(Url));	
		Port -> binary_to_integer(Port)
	end.
			
path(Url) ->
	proplists:get_value(<<"Path">>,Url).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test() ->
	?assertEqual([{<<"Host">>,<<"bedrock-runtime.us-east-1.amazonaws.com">>},
	 {<<"Path">>,
	  <<"/model/anthropic.claude-3-5-sonnet-20240620-v1:0/invoke">>},
	 {<<"Port">>,<<"443">>},
	 {<<"Proto">>,<<"https">>},
	 {<<"Query">>,<<>>}],
 	url:parse("https://bedrock-runtime.us-east-1.amazonaws.com:443/model/anthropic.claude-3-5-sonnet-20240620-v1:0/invoke")),
	?assertEqual([{<<"Host">>,<<"bedrock-runtime.us-east-1.amazonaws.com">>},
	 {<<"Path">>,
	  <<"/model/anthropic.claude-3-5-sonnet-20240620-v1:0/invoke">>},
	 {<<"Port">>,<<"443">>},
	 {<<"Proto">>,<<"https">>},
	 {<<"Query">>,<<"?foo=bar">>}],
 	url:parse("https://bedrock-runtime.us-east-1.amazonaws.com:443/model/anthropic.claude-3-5-sonnet-20240620-v1:0/invoke?foo=bar")).


-endif.

