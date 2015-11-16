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
	case Req:query_param("status", "pending") of
		"pending" ->
			Downloads = boss_db:find(download, 
									 [{status, in, [?DL_PENDING, ?DL_ACQUIRED, ?DL_ACTIVE, ?DL_PAUSED]}], 
									 [{order_by, status}, {descending, true}]),
			{ok, [{status_type, "pending"}, {downloads, Downloads}]};
		"completed" ->
			Downloads = boss_db:find(download, 
									 [{status, equals, ?DL_COMPLETED}], 
									 [{order_by, status}, {descending, true}]),
			{ok, [{status_type, "completed"}, {downloads, Downloads}]};
		"failed" ->
			Downloads = boss_db:find(download, 
									 [{status, in, [?DL_FAILED, ?DL_NOT_FOUND]}], 
									 [{order_by, status}, {descending, true}]),
			{ok, [{status_type, "failed"}, {downloads, Downloads}]}
	end.

