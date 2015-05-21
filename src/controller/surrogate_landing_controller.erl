

-module(surrogate_landing_controller, [Req]).
-compile(export_all).

config('GET', []) ->
    Configs = boss_db:find(config, []),
    {ok, []};
	%case boss_db:find(config, [{id, 1}], [{limit,1}]) of
		%[Config] ->
			%{redirect, proplists:get_value("redirect", Req:post_params(), "/login"), user:login_cookies()};
		%[] -> 
			%{output, {"Hello World"}};
	%end;
%end.

config('POST', []) ->
	{output, []}.