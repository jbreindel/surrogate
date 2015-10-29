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
-export(config/2).

config('GET', []) ->
	case boss_db:find(config, [], [{limit, 1}]) of
		[Config] ->
			{redirect, "/login", []};
		[] -> 
			Config = config:new(id, "/opt/surrogate/", 3),
			{ok, [{config, Config}]}
	end;

config('POST', []) ->
	Account = account:new(id, Req:post_param("userName"), account_lib:create_password_hash(Req:post_param("password"), Req:post_param("userName"))),
	{NumDownloads, _} = string:to_integer(Req:post_param("numSimultaneousDownloads")),
	Config = config:new(id, Req:post_param("downloadDirectory"), NumDownloads),
	case Account:save() of
        {ok, SavedAccount} ->
			case Config:save() of
				{ok, SavedConfig} ->
					erlang:display("Saved Config!"),
            		{redirect, "/login", []};				
				{error, Errors} ->
					erlang:display({config_save, Errors}),
				   	{ok, [{errors, Errors}, {config, Config}]}
			end;
        {error, Errors} ->
        	{ok, [{errors, Errors}, {config, Config}]}
    end.