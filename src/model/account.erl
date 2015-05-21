%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: account.erl
%
% AUTHOR: Jake Breindel
% DATE: 5-20-15
%
% DESCRIPTION:
%
% Model class for account
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(account, [Id, UserName, PasswordHash]).
-compile(export_all).
-has({rg_account, one}).
-has({downloads, many}).

-define(SECRET_STRING, "SecretString!!").

session_identifier() ->
    mochihex:to_hex(erlang:md5(?SECRET_STRING ++ Id)).

check_password(Password) ->
    Salt = mochihex:to_hex(erlang:md5(UserName)),
    user_lib:hash_password(Password, Salt) =:= PasswordHash.

login_cookies() ->
    [ mochiweb_cookies:cookie("account_id", Id, [{path, "/login"}]),
        mochiweb_cookies:cookie("session_id", session_identifier(), [{path, "/login"}]) ].