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
	Offset = Req:query_param("offset", 0),
	Limit = Req:query_param("limit", 15),
	case Req:query_param("filter", "pending") of
		"pending" ->
			Downloads = boss_db:find(download, 
									 [{status, in, [?DL_PENDING, ?DL_ACQUIRED, ?DL_ACTIVE, ?DL_PAUSED]}], 
									 [{limit, Limit}, {order_by, status}, {descending, true}]),
			{json, Downloads};
		"completed" ->
			Downloads = boss_db:find(download, 
									 [{status, equals, ?DL_COMPLETED}], 
									 [{limit, Limit}, {order_by, status}, {descending, true}]),
			{json, Downloads};
		"failed" ->
			Downloads = boss_db:find(download, 
									 [{status, in, [?DL_FAILED, ?DL_NOT_FOUND]}], 
									 [{limit, Limit}, {order_by, status}, {descending, true}]),
			{json, Downloads}
	end.

