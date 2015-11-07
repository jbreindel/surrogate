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
	list_to_atom(Account:id() ++ "-http-profile").

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
	case alive(Account) of
		false ->
			case inets:start(httpc, [{cookies, enabled}, {profile, HttpProfileName}], stand_alone) of
				{ok, HttpPid} ->
					erlang:display({http_profile_info, httpc:info(HttpPid)});
				{error, already_started} ->
					erlang:display({http_profile_started, started})
			end;
		Pid ->
			Pid
	end.

