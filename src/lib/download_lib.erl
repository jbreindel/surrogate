%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: download_lib.erl
%
% AUTHOR: Jake Breindel
% DATE: 5-25-15
%
% DESCRIPTION:
%
% Download library
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(download_lib).
-compile(export_all).
-include("download_status.hrl").

save_downloads(Premium, Links) ->
	save_downloads(Premium, Links, []).

save_downloads(Premium, [], SavedDls) ->
	{ok, SavedDls};
save_downloads(Premium, [Link|Links], SavedDls) ->
	RealUrl = "",
	File = "",
	Length = 0,
	Progress = 0,
	CreatedTime = os:timestamp(),
	Download = download:new(id, Link, RealUrl, ?DL_PENDING, File, Length, Progress, CreatedTime, Premium:id()),
	case Download:save() of
		{ok, SavedDownload} ->
			SavedDlList = SavedDls ++ [SavedDownload],
    		save_downloads(Premium, Links, SavedDlList);				
		{error, Errors} ->
			{error, Errors}
	end.
	