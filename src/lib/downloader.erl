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

execute(Account, Download) ->
	HttpClient = http_client:instance(Account),
	Headers = [{"User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.80 Safari/537.36"},
		   {"Accept-Language", "en-US,en;q=0.8"}],
	case httpc:request(get, {Download:real_url(), Headers}, [], [{sync, false}, {stream, self}, {receiver, self()}, {body_format, binary}], HttpClient) of
		{ok, RequestId} ->
			receive
				{http, {RequestId, stream_start, Headers}} ->
					erlang:display({stream_start, Headers});
				{http, {RequestId, stream, BinBodyPart}} ->
					erlang:display({stream, RequestId});
				{http, {RequestId, stream_end, Headers}} ->
					erlang:display({stream_end, Headers})
			end
	end.