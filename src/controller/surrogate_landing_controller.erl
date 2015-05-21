%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: surrogate_landing_controller.erl
%
% AUTHOR: Jake Breindel
% DATE: 5-20-15
%
% DESCRIPTION:
%
% Controller for the landing page.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(surrogate_landing_controller, [Req]).
-compile(export_all).

config('GET', []) ->
	case boss_db:find(config, [{id, 1}], [{limit,1}]) of
		[Config] ->
			{redirect, proplists:get_value("redirect", Req:post_params(), "/login/login"), account:login_cookies()};
		[] -> 
			{ok, []}
	end;

config('POST', []) ->
 	{ok, []}.