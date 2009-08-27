-module (config_reader).

-export ([get_config/1, get_config2/1]).

-include("config.hrl").




%%-----------------
% reads the cfg file and puts in a record
%%-----------------
get_config(File) ->
	{ok, C} = file:consult(File),
	#config{host=proplists:get_value(host, C), port=proplists:get_value(port, C), username=proplists:get_value(username, C), 
		password=proplists:get_value(password, C), from=proplists:get_value(from, C), to=proplists:get_value(to, C), header_to=proplists:get_value(header_to, C)}.
	

get_config2(File) ->
	{ok, C} = file:consult(File),
	Dict = dict:new(),
	get_config(C, Dict).
get_config([], Dict) -> Dict;
get_config([{Profile, Properties}|T], Dict) ->
	C = #config{host=proplists:get_value(host, Properties), port=proplists:get_value(port, Properties), username=proplists:get_value(username, Properties), 
		password=proplists:get_value(password, Properties), from=proplists:get_value(from, Properties), to=proplists:get_value(to, Properties), header_to=proplists:get_value(header_to, Properties)},
	get_config(T, dict:store(Profile, C, Dict)).