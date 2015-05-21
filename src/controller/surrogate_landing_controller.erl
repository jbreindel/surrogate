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
			%% Create new config
			{ok, []}
	end;

config('POST', []) ->
	Account = account:new(id, Req:post_param("UserName"), account_lib:create_password_hash(Req:post_param("Password"))),
	case Account:save() of
        {ok, SavedAccount} -> 
			%% Create config
            {redirect, "/voter/view/"};
        {error, Errors} ->
			%% Create new config
           {ok, [{errors, Errors}, {config, Config}]}
    end.