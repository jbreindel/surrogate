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
			{redirect, "/login/login", account:login_cookies()};
		[] -> 
			Config = config:new(id, false, "/opt/surrogate", 3),
			{ok, [{config, Config}]}
	end;

config('POST', []) ->
	Config = config:new(id, Req:post_param("autoStart"), Req:post_param("downloadDirectory"), Req:post_param("numSimultaneousDownloads")),
	Account = account:new(id, Req:post_param("userName"), account_lib:create_password_hash(Req:post_param("password"), Req:post_param("userName"))),
	case Account:save() of
        {ok, SavedAccount} ->
			case Config:save() of
				{ok, SavedConfig} ->
            		{redirect, "/login/login/", []};				
				{error, Errors} ->
				   {ok, [{errors, Errors}, {config, Config}]}
			end;
        {error, Errors} ->
           {ok, [{errors, Errors}, {config, Config}]}
    end.