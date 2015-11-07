%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: http_client.erl
%
% AUTHOR: Jake Breindel
% DATE: 11-7-15
%
% DESCRIPTION:
%
% Opens http profiles for accounts.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(http_client).
-export([pid_name/1, alive/1, instance/1, display_cookies/1]).

pid_name(Account) ->
	list_to_atom(Account:id() ++ "-http-profile").

alive(Account) ->
	HttpClientName = pid_name(Account),
	case whereis(HttpClientName) of
		undefined ->
			false;
		Pid ->
			Pid
	end.

instance(Account) ->
	HttpProfileName = pid_name(Account),
	case alive(Account) of
		false ->
			case inets:start(httpc, [{profile, HttpProfileName}, {verbose, verbose}], stand_alone) of
				{ok, HttpPid} ->
					httpc:set_options([{cookies, verify}], HttpPid),
					erlang:display({http_profile_info, httpc:info(HttpPid)}),
					register(HttpProfileName, HttpPid),
					HttpPid;
				{error, already_started} ->
					erlang:display({http_profile_started, started}),
					undefined
			end;
		Pid ->
			Pid
	end.

display_cookies(Account) ->
	HttpClient = instance(Account),
	erlang:display({client_info, httpc:info(HttpClient)}),
	case httpc:which_cookies(pid_name(Account)) of
		[Cookie|Cookies] ->
			erlang:display({cookie, Cookie});
		Cooky ->
			erlang:display({display_cookies, Cooky})
	end.
