%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: acquirer.erl
%
% AUTHOR: Jake Breindel
% DATE: 6-4-15
%
% DESCRIPTION:
%
% Acquirer that acquires actual download
% links.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(acquirer).
-export([acquire/2, acquire_downloads/2]).
-include("download_status.hrl").

url_end_index(UrlStart, Body) ->
	case lists:nth(UrlStart, Body) of
		Char when Char =:= $' ->
			UrlStart - 1;
		Char ->
			url_end_index(UrlStart + 1, Body)
	end.

parse_download_url(Body) ->
	%% TODO maybe regex here instead? (?:var\spremium_download_link\s=\s')(.*)(?:';)
	case string:str(Body, "premium_download_link") of
		0 ->
			undefined;
		Index ->
			UrlStartIndex = Index + string:len("premium_download_link = '"),
			UrlEndIndex = url_end_index(UrlStartIndex, Body),
			string:sub_string(Body, UrlStartIndex, UrlEndIndex)
	end.
	
acquire(Account, Download) ->
	%% TODO differentiate different downloads
	HttpClient = http_client:instance(Account),
	Headers = [{"Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"}, 
			   {"User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.80 Safari/537.36"},
			   {"Accept-Language", "en-US,en;q=0.8"}],
	case httpc:request(get, {Download:display_url(), Headers}, [], [], HttpClient) of
		{ok, {{Version, 200, ReasonPhrase}, RespHeaders, Body}} ->
			ManagerName = manager:pid_name(Account),
			case parse_download_url(Body) of
				undefined ->
					process_lib:cond_send(Account, {download_not_found, [{download, Download}]});
				RealUrl ->
					erlang:display({download_acquired, [{real_url, RealUrl}, {account, Account}]}),
					process_lib:cond_send(Account, {download_acquired, [{download, Download}, {real_url, RealUrl}]})
			end;
		Request ->
			erlang:display(Request),
			error
	end.

acquire_downloads(Account, []) ->
	erlang:display({acquire_download, []}),
	ok;
acquire_downloads(Account, [Download|Downloads]) ->
	erlang:display({acquire_downloads, Downloads}),
	case Download:status() of
		?DL_PENDING ->
			erlang:display({status_pend, 0}),
			erlang:spawn(?MODULE, acquire, [Account, Download]);
		Status ->
			erlang:display({status, Status})
	end,
	acquire_downloads(Account, Downloads).