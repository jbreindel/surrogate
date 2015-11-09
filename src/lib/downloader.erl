%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Downloader.erl
%
% AUTHOR: Jake Breindel
% DATE: 11-8-15
%
% DESCRIPTION:
%
% Downloads the file from the real url.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(downloader).
-export([execute/2]).

parse_file_name(FileName) ->
	case string:str(FileName, "\"") of
		0 ->
			FileName;
		Index ->
			string:substr(FileName, 0, Index - 1)
	end.

download_meta(HttpClient, Download) ->
	Headers = [{"User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.80 Safari/537.36"},
		   {"Accept-Language", "en-US,en;q=0.8"}],
	case httpc:request(head, {Download:real_url(), Headers}, [], [], HttpClient) of
		{ok, {{Version, 200, ReasonPhrase}, RespHeaders, Body}} ->
			ContentLengthStr = proplists:get_value("content-length", RespHeaders),
			{ContentLength, _} = string:to_integer(ContentLengthStr),
			ContentDispositionStr = proplists:get_value("content-disposition", RespHeaders),
			[_, FileNameStr] = string:tokens(ContentDispositionStr, "=\""),
			FileName = parse_file_name(FileNameStr),
			MetaDownload = Download:set([{file, FileName}, {length, ContentLength}]),
			case MetaDownload:save() of
				{ok, SavedDownload} ->
					{download, SavedDownload};
				{error, Errors} ->
					{error, Errors}
			end;
		Response ->
			{error, Response}
	end.

download_file(Download) ->
	case config:find_first() of
		undefined ->
			{error, "Cannot find Config"};
		Config ->
			Config:download_directory() ++ Download:file()
	end.

download(Download, SpeedOrddict) ->
	receive
		{http, {RequestId, stream_start, Headers}} ->
			erlang:display({stream_start, Headers}),
			download(Download, SpeedOrddict);
		{http, {RequestId, stream, BinBodyPart}} ->
			TimeMs = date_lib:epoch_hires(),
%% 			orddict:store(TimeMs, [{length, length(BinBodyPart)}], SpeedOrddict),
%% 			file:write_file(Download:file(), BinBodyPart, append),
			erlang:display([{stream, RequestId}, {time, TimeMs}, {length, byte_size(BinBodyPart)}]),
			download(Download, SpeedOrddict);
		{http, {RequestId, stream_end, Headers}} ->
			erlang:display({stream_end, Headers})
	end.

execute(Account, Download) ->
	HttpClient = http_client:instance(Account),
	case download_meta(HttpClient, Download) of
		{download, UpdatedDownload} ->
			Headers = [{"User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.80 Safari/537.36"},
				   {"Accept-Language", "en-US,en;q=0.8"}],
			case httpc:request(get, {UpdatedDownload:real_url(), Headers}, [], [{sync, false}, {stream, self}, {receiver, self()}, {body_format, binary}], HttpClient) of
				{ok, RequestId} ->
					download(UpdatedDownload, orddict:new())
			end;
		{error, Errors} ->
			erlang:display({content_disposition, Errors})
	end.