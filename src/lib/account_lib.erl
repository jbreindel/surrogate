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

create_password_hash(Password) ->
    Salt = mochihex:to_hex(erlang:md5(UserName)),
    Hash = user_lib:hash_password(Password, Salt),
    Hash.