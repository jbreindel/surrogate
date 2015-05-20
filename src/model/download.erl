%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: download.erl
%
% AUTHOR: Jake Breindel
% DATE: 5-20-15
%
% DESCRIPTION:
%
% Model class for a download
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(download, [Id, DisplayUrl, RealUrl, Status, File, UserId]).
-compile(export_all).
-belongs_to(user).