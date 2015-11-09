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

notify_manager(Account, Data) ->
	case manager:alive(Account) of
		false ->
			false;
		Pid ->
			Pid ! Data
	end.

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

list_min([H|T]) -> 
	list_min(H, T).

list_min(X, []) -> 
	X;
list_min(X, [H|T]) -> 
	list_min(erlang:min(H, X), T).

list_max([H|T]) -> 
	list_max(H, T).

list_max(X, []) -> 
	X;
list_max(X, [H|T]) -> 
	list_max(erlang:max(H, X), T).

calc_byte_sum([], Sum) ->
	Sum;
calc_byte_sum([Elem|Elems], Sum) ->
	{_, LengthProps} = Elem,
	calc_byte_sum(Elems, Sum + proplists:get_value(length, LengthProps)).

update_speed(Account, Download, SpeedOrddict, Length) ->
	TimeMs = date_lib:epoch_hires(),
	Timestamps = orddict:fetch_keys(SpeedOrddict),
	EarliestTimeMs = list_min(Timestamps),
	case TimeMs - EarliestTimeMs of
		Diff when Diff < 1000 ->
			orddict:store(TimeMs, [{length, Length}], SpeedOrddict);
		Diff when Diff >= 1000 ->
			ByteCount = calc_byte_sum(SpeedDict:to_list(), 0),
			TimeDiff = list_max(Timestamps) - list_min(Timestamps),
			case subscriber:alive(Account) of
				false ->
					orddict:store(TimeMs, [{length, Length}], orddict:new());
				SubscriberPid ->
					case manager:alive(Account) of
						false ->
							orddict:store(TimeMs, [{length, Length}], orddict:new());
						ManagerPid ->
							ManagerPid ! {download_progress, [{download, Download}, {speed, ByteCount / TimeDiff}, {progress, ByteCount}]},
							orddict:store(TimeMs, [{length, Length}], orddict:new())
					end
			end
	end.

download(Account, Download, SpeedOrddict) ->
	receive
		{http, {RequestId, stream_start, Headers}} ->
			erlang:display({stream_start, Headers}),
			notify_manager(Account, {download_started, [{download, Download}]}),
			download(Account, Download, SpeedOrddict);
		{http, {RequestId, stream, BinBodyPart}} ->
			UpdatedSpeedOrdict = update_speed(Account, Download, SpeedOrddict, byte_size(BinBodyPart)),
 			file:write_file(Download:file(), BinBodyPart, append),
			download(Account, Download, UpdatedSpeedOrdict);
		{http, {RequestId, stream_end, Headers}} ->
			erlang:display({stream_end, Headers}),
			notify_manager(Account, {download_started, [{download, Download}]}),
	end.

execute(Account, Download) ->
	HttpClient = http_client:instance(Account),
	case download_meta(HttpClient, Download) of
		{download, UpdatedDownload} ->
			Headers = [{"User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.80 Safari/537.36"},
				   {"Accept-Language", "en-US,en;q=0.8"}],
			case httpc:request(get, {UpdatedDownload:real_url(), Headers}, [], [{sync, false}, {stream, self}, {receiver, self()}, {body_format, binary}], HttpClient) of
				{ok, RequestId} ->
					download(Account, UpdatedDownload, orddict:new())
			end;
		{error, Errors} ->
			erlang:display({content_disposition, Errors})
	end.