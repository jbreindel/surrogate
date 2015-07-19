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

-module(download, [Id, DisplayUrl::string(), RealUrl::string(), Status::integer(), Process, File::string(), Length::integer(), Progress::integer(), PremiumId::integer()]).
-compile(export_all).
-belongs_to(premium).

-define(DL_PENDING, 0).
-define(DL_AQUIRED, 1).
-define(DL_ACTIVE, 2).
-define(DL_PAUSED, 3).
-define(DL_COMPLETED, 4).
-define(DL_FAILED, 5).
-define(DL_NOT_FOUND, 6).