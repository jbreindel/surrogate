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
-export([premium_login/2]).

rapidgator_login_form(Email, Password) ->
	UrlEmail = edoc_lib:escape_uri(Email),
	UrlPassword = edoc_lib:escape_uri(Password),
	"LoginForm%5Bemail%5D=" ++ UrlEmail ++ 
	"&LoginForm%5Bpassword%5D=" ++ UrlPassword ++ 
	"&LoginForm%5BrememberMe%5D=0&LoginForm%5BrememberMe%5D=1&LoginForm%5BverifyCode%5D=".
		

rapidgator_login(Account, Premium) ->
	erlang:display(Premium),
	Form = rapidgator_login_form(Premium:user_name(), Premium:password()),
	HttpClient = http_client:instance(Account),
	case httpc:request(post, {"https://rapidgator.net/auth/login", [], 
							  "application/x-www-form-urlencoded", Form}, [], [], HttpClient) of
		{ok, {{Version, 302, ReasonPhrase}, RespHeaders, Body}} ->
			true;
		Response ->
			erlang:display(Response)
	end.
	

premium_login(Account, Premium) ->
	case Premium:type() of
		"Rapidgator" ->
			rapidgator_login(Account, Premium);
		_ ->
			false
	end.
			