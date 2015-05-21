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

-module(download, [Id, DisplayUrl, RealUrl, Status, Process, File, Length, Progress, PremiumId]).
-compile(export_all).
-belongs_to(premium).