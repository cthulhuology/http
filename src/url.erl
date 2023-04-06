
-module(url).
-export([ parse/1, host/1, port/1, path/1, protocol/1 ]).


parse(Url) ->
	{ ok, M } = re:compile("(?<Proto>[^:]+)://(?<Host>[^:/]+)(:(?<Port>[^/]+))?(?<Path>/.*)"),
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


