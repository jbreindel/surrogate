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

config_form_validator() ->
	[
	 [{name, "userName"}, 
	 {rule, fun(UserName) ->
				case form_lib:str_len_validator(UserName, 
				[{min, 2}, {message, "UserName must be greater than 2 characters"}], 
				[{max, 32}, {message, "Username can't be more than 32 characters."}]) of
					Message ->
						Message;
					undefined ->
						true
				end 
	  		end}],
	[{name, "password"},
	{rule, fun(Password) -> 
				case form_lib:str_len_validator(Password, 
				[{min, 8}, {message, "Password must be between 8 and 32 characters."}], 
				[{max, 32}, {message, "Password can't be more than 32 characters."}]) of
					Message ->
						Message;
					undefined ->
						true
				end
	 		end}]
	].

config('GET', []) ->
	case boss_db:find(config, [], [{limit, 1}]) of
		[Config] ->
			{redirect, "/login/login", []};
		[] -> 
			Config = config:new(id, "/opt/surrogate/", 3),
			{ok, [{config, Config}]}
	end;

config('POST', []) ->
	{NumDownloads, _} = string:to_integer(Req:post_param("numSimultaneousDownloads")),
	Config = config:new(id, Req:post_param("downloadDirectory"), NumDownloads),
	FormValidator = config_form_validator(),
	case form_lib:validate(Req, FormValidator) of
		[] ->
			Account = account:new(id, Req:post_param("userName"), account_lib:create_password_hash(Req:post_param("password"), Req:post_param("userName"))),
			case Account:save() of
		        {ok, SavedAccount} ->
					case Config:save() of
						{ok, SavedConfig} ->
		            		{redirect, "/login/login", []};				
						{error, Error} ->
						   	{ok, [{error, Error}, {config, Config}]}
					end;
		        {error, Error} ->
		        	{ok, [{error, Error}, {config, Config}]}
		    end;
		Error ->
			{ok, [{error, Error}, {config, Config}]}
	end.
	