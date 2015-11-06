%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: download_status.erl
%
% AUTHOR: Jake Breindel
% DATE: 7-19-15
%
% DESCRIPTION:
%
% Download statuses.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(DL_PENDING, 0).
-define(DL_ACQUIRED, 1).
-define(DL_ACTIVE, 2).
-define(DL_PAUSED, 3).
-define(DL_COMPLETED, 4).
-define(DL_FAILED, 5).
-define(DL_NOT_FOUND, 6).