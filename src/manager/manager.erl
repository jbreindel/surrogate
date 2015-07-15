%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: manager.erl
%
% AUTHOR: Jake Breindel
% DATE: 5-26-15
%
% DESCRIPTION:
%
% Manager class that runs in seperate
% process and manages download events.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(manager).
-export(loop/1).

loop(Account) ->
	loop(Account, [], undefined, undefined);

loop(Account, ActiveDownloads, Subscriber, DownloadStatus) ->
	recieve

		%%%%%%%%%%%%%%%%%%%%%
		%% Client Messages %%
		%%%%%%%%%%%%%%%%%%%%%

		{subscriber_connect, Subscriber} ->
			loop(Account, ActiveDownloads, Subscriber, );

		{change_download_status, DownloadStatus}
			%% TODO
			loop(Account, ActiveDownloads, Subscriber, DownloadStatus);

		{links, Links} ->
			handle_links(Account, Links),
			loop(Account, ActiveDownloads, Subscriber);

		{subscriber_disconnect, _} ->
			loop(Account, ActiveDownloads, undefined).

%% handle_links(Account, Links) ->
%% 	case download_lib:save_downloads(Account:premium(), Links, []) of
%% 		{ok, SavedDownloads} ->
%% 			
%% 		{error, Errors} ->
%% 			
%% 	end;