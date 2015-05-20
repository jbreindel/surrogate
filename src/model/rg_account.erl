%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: rg_account.erl
%
% AUTHOR: Jake Breindel
% DATE: 5-20-15
%
% DESCRIPTION:
%
% Model class for rapidgator account
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(rg_account, [Id, UserName, Password, UserId]).
-compile(export_all).
-belongs_to(user).