%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: account_lib.erl
%
% AUTHOR: Jake Breindel
% DATE: 5-21-15
%
% DESCRIPTION:
%
% Account library
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(account_lib).
-compile(export_all).

hash_password(Password, Salt) ->
    mochihex:to_hex(erlang:md5(Salt ++ Password)).

create_password_hash(Password, UserName) ->
    Salt = mochihex:to_hex(erlang:md5(UserName)),
    hash_password(Password, Salt).

require_login(Req) ->
	case Req:cookie("account_id") of
		undefined -> 
			{redirect, "/login/login"};
		Id ->
			case boss_db:find(Id) of
                undefined -> 
					{redirect, "/"};
                Account ->
					case Account:session_identifier() =:= Req:cookie("session_id") of
						false ->
							erlang:display({false, Account}),
							{redirect, "/login/login"};
						true ->
							erlang:display({true, Account}),
							{ok, Account}
					end
			end
	end.