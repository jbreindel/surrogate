%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: premium_lib.erl
%
% AUTHOR: Jake Breindel
% DATE: 10-31-15
%
% DESCRIPTION:
%
% Library for premium functions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(premium_lib).
-export([premium_login/1]).

rapidgator_login_form(Email, Password) ->
	UrlEmail = edoc_lib:escape_uri(Email),
	UrlPassword = edoc_lib:escape_uri(Password),
	"LoginForm%5Bemail%5D=" ++ UrlEmail ++ 
	"&LoginForm%5Bpassword%5D=" ++ UrlPassword ++ 
	"&LoginForm%5BrememberMe%5D=0&LoginForm%5BrememberMe%5D=1&LoginForm%5BverifyCode%5D=".
		

rapidgator_login(Premium) ->
	erlang:display(Premium),
	Form = rapidgator_login_form(Premium:user_name(), Premium:password()),
	case httpc:request(post, {"https://rapidgator.net/auth/login", [], 
							  "application/x-www-form-urlencoded", Form}, [], []) of
		{ok, {{Version, 302, ReasonPhrase}, Headers, Body}} ->
			true;
		true ->
			false
	end.
	

premium_login(Premium) ->
	case Premium:type() of
		"Rapidgator" ->
			rapidgator_login(Premium);
		_ ->
			false
	end.
			