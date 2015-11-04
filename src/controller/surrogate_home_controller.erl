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
-include("src/lib/download_status.hrl").

before_(_) ->
    account_lib:require_login(Req).

home('GET', [], Account) ->
	ManagerName = manager:pid_name(Account),
	case whereis(ManagerName) of
		undefined ->
			spawn(manager, loop, [Account]),
			{ok, [{account, Account}]}
		Pid ->
			{ok, [{account, Account}]}
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