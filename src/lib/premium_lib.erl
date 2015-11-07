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

cookie_tokens_header(CookieHeaders) ->
	cookie_tokens_header("", CookieHeaders).

cookie_tokens_header(Cookie, []) ->
	Cookie;
cookie_tokens_header(Cookie, [CookieHeader|CookieHeaders]) ->
	case string:str(CookieHeader, "expires") of
		0 when Cookie =:= "" ->
			case string:str(CookieHeader, "domain") of
				0 when Cookie =:= "" ->
					cookie_tokens_header(CookieHeader, CookieHeaders);
				0 ->
					cookie_tokens_header(Cookie ++ "; " ++ CookieHeader, CookieHeaders);
				Index ->
					cookie_tokens_header(Cookie, CookieHeaders)
			end;
		0 ->
			case string:str(CookieHeader, "domain") of
				0 when Cookie =:= "" ->
					cookie_tokens_header(CookieHeader, CookieHeaders);
				0 ->
					cookie_tokens_header(Cookie ++ "; " ++ CookieHeader, CookieHeaders);
				Index ->
					cookie_tokens_header(Cookie, CookieHeaders)
			end;
		Index ->
			case string:str(CookieHeader, "domain") of
				0 when Cookie =:= "" ->
					cookie_tokens_header(CookieHeader, CookieHeaders);
				0 ->
					cookie_tokens_header(Cookie ++ "; " ++ CookieHeader, CookieHeaders);
				Index ->
					cookie_tokens_header(Cookie, CookieHeaders)
			end
	end.

cookie_headers(CookieHeaders) ->
	cookie_headers(CookieHeaders, []).

cookie_headers([], Headers)->
	Headers;
cookie_headers([Cookie|Cookies], Headers) ->
	CookieTokens = string:tokens(Cookie, ";"),
	ParsedCookie = cookie_tokens_header(CookieTokens),
	cookie_headers(Cookies, Headers ++ [{"set-cookie", ParsedCookie}]).

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
			Cookies = proplists:get_all_values("set-cookie", RespHeaders),
			CookieHeaders = cookie_headers(Cookies),
			httpc:store_cookies(CookieHeaders, "http://rapidgator.net", HttpClient);
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
			