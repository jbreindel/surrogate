%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: download_updater.erl
%
% AUTHOR: Jake Breindel
% DATE: 11-30-15
%
% DESCRIPTION:
%
% Updates download progress.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(download_updater).
-export([update_download/2]).

update_download(Account, Download) ->
	FileSize = filelib:file_size(Download:file()),
	TimeMs = date_lib:now_to_milliseconds_hires(now()),
	update_download(Account, Download, [{time, TimeMs}, {length, FileSize}]).

update_download(Account, Download, DownloadProgress) ->
	erlang:send_after(1000, self(), {download_progress, DownloadProgress}),
	ManagerName = manager:pid_name(Account),
	receive
		{download_progress, DownloadProgress} ->
			NowFileSize = filelib:file_size(Download:file()),
			NowTimeMs = date_lib:now_to_milliseconds_hires(now()),
			LastFileSize = proplists:get_value(length, DownloadProgress),
			LastTimeMs = proplists:get_value(time, DownloadProgress),
			ByteCount = NowFileSize - LastFileSize,
			DeltaTime = NowTimeMs - LastTimeMs,
			%%UpdatedDownload = Download:update(),
			erlang:display([{download, Download}, {speed, ByteCount / DeltaTime}, {chunk_size, ByteCount}]),
			process_lib:find_send(ManagerName, {download_progress,
												[{download, Download}, 
											  		{speed, ByteCount / DeltaTime}, 
											  		{chunk_size, ByteCount}]}),
			update_download(Account, Download,
							[{time, NowTimeMs}, {length, NowFileSize}]);
		Message ->
			erlang:display({download_updater_message, Message})
	end.
					