%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: surrogate_download_controller.erl
%
% AUTHOR: Jake Breindel
% DATE: 
%
% DESCRIPTION:
%
% Rest controller for downloads.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(surrogate_download_controller, [Req]).
-compile(export_all).
-include("src/lib/download_status.hrl").

before_(_) ->
    account_lib:require_login(Req).

downloads('GET', [], Account) ->
	Premium = Account:premium(),
	case Req:query_param("filter", "pending") of
		"pending" ->
			{ok, [{downloads, Premium:downloads({status, 'in', [?DL_PENDING, ?DL_AQUIRED, ?DL_ACTIVE, ?DL_PAUSED]})}]};
		"completed" ->
			{ok, [{downloads, Premium:downloads({status, 'equals', ?DL_COMPLETED})}]};
		"failed" ->
			{ok, [{downloads, Premium:downloads({status, 'in', [?DL_FAILED, ?DL_NOT_FOUND]})}]}
	end;

downloads('POST', [], Account) ->
	{ok, []}.

