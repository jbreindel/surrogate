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
	case Req:query_param("filter") of
		"pending" ->
			Downloads = Premium:downloads({status, [0, 1, 2]});
		"completed" ->
			Downloads = Premium:downloads({status, 3});
		"failed" ->
			Downloads = Premium:downloads({status, 4});
		undefined ->
			Downloads = Premium:downloads({status, [0, 1, 2]})
	end,
	{ok, Downloads};

home('POST', [], Account) ->
	Dls = Req:post_param("downloads").