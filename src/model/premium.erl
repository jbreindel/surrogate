%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: premium.erl
%
% AUTHOR: Jake Breindel
% DATE: 5-20-15
%
% DESCRIPTION:
%
% Model class for rapidgator account
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(premium, [Id, Type::string(), UserName::string(), Password::string(), AccountId::integer()]).
-compile(export_all).
-belongs_to(account).
-has({downloads, many}).