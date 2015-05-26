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

save_downloads(Premium, [], SavedDls) ->
	{ok, lists:reverse(SavedDls)};

save_downloads(Premium, [Link | Links], SavedDls) ->
	Download = download:new(id, Link, "", ?DL_PENDING, "", "", 0, 0, Premium:id()),
	case Download:save() of
		{ok, SavedDownload} ->
			SavedDlList = [SavedDownload | SavedDls],
    		save_downloads(Account, Links, SavedDlList);				
		{error, Errors} ->
		   {error, Errors}
	end.		