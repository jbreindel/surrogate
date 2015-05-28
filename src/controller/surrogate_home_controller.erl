%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: surrogate_home_controller.erl
%
% AUTHOR: Jake Breindel
% DATE: 5-21-15
%
% DESCRIPTION:
%
% Controller for the home page.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(surrogate_home_controller, [Req]).
-compile(export_all).

before_(_) ->
    account_lib:require_login(Req).

home('GET', [], Account) ->
	Premium = Account:premium(),
	case Req:query_param("filter", "pending") of
		"pending" ->
			{ok, {downloads, Premium:downloads({status, 'in', [?DL_PENDING, ?DL_AQUIRED, ?DL_ACTIVE, ?DL_PAUSED]})}};
		"completed" ->
			{ok, {downloads, Premium:downloads({status, 'equals', ?DL_COMPLETED})}};
		"failed" ->
			{ok, {downloads, Premium:downloads({status, 'in', [?DL_FAILED, ?DL_NOT_FOUND]})}}
	end;

home('POST', [], Account) ->
	Dls = Req:post_param("downloads"),
	Links = string:tokens(Dls, "\n"),
	case download_lib:save_downloads(Account:premium(), Links, []) of
		{ok, SavedDownloads} ->
			%% TODO spawn aquisition process
			%% flash message
			{redirect, "/home/"};
		{error, Errors} ->
			%% flash message
			{redirect, "/home/"}
	end.