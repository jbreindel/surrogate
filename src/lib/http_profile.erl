%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: http_profile.erl
%
% AUTHOR: Jake Breindel
% DATE: 11-7-15
%
% DESCRIPTION:
%
% Opens http profiles for accounts.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(http_profile).
-export([pid_name/1, alive/1, start/1]).

pid_name(Account) ->
	Account:id() ++ "-http-profile";

alive(Account) ->
	HttpProfileName = pid_name(Account),
	case whereis(HttpProfileName) of
		undefined ->
			false;
		Pid ->
			Pid
	end.

start(Account) ->
	HttpProfileName = pid_name(Account),
	case alive(HttpProfileName) of
		false ->
			HttpPid = inets:start([{profile, HttpProfileName}]),
			httpc:set_options([{cookies, enabled}], HttpPid),
			register(HttpProfileName, HttpPid);
		Pid ->
			Pid
	end.

