%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: surrogate_settings_controller.erl
%
% AUTHOR: Jake Breindel
% DATE: 10-30-15
%
% DESCRIPTION:
%
% Controller for settings form.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(surrogate_settings_controller, [Req]).
-compile(export_all).

before_(_) ->
    account_lib:require_login(Req).

settings('GET', [], Account) ->
	{ok, [{account, Account}]};

settings('POST', [], Account) ->
	erlang:display([{account_id, Account:id()}, {email, Req:post_param("email")}, {password, Req:post_param("password")}]),
	Premium = premium:new(id, "Rapidgator", Req:post_param("email"), Req:post_param("password"), Account:id()),
	case premium_lib:premium_login(Premium) of
		true ->
			case Premium:save() of
				{ok, SavedPremium} ->
					%% TODO add to flashbag
					{redirect, "/settings/settings"};
		        {error, Error} ->
					{ok, [{error, Error}]}
			end;
		false ->
			{ok, [{error, [{emailError, "Unable to login to account"}]}]}
	end.