-module (config_reader).

-export ([get_config/1]).

-include("config.hrl").




%%-----------------
% reads the cfg file and puts in a record
%%-----------------
get_config(File) ->
	{ok, C} = file:consult(File),
	#config{host=proplists:get_value(host, C), port=proplists:get_value(port, C), username=proplists:get_value(username, C), 
		password=proplists:get_value(password, C), from=proplists:get_value(from, C), to=proplists:get_value(to, C), header_to=proplists:get_value(header_to, C)}.
	

	