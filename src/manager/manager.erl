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
	loop(Account, [], undefined);

loop(Account, ActiveDownloads, Websocket) ->
	recieve

		%%%%%%%%%%%%%%%%%%%%%
		%% Client Messages %%
		%%%%%%%%%%%%%%%%%%%%%

		{ws_connect, WebSocket} ->
			loop(Account, ActiveDownloads, Websocket);

		{links, Links} ->
			handle_links(Account, Links);

		{ws_disconnect, _} ->
			loop(Account, ActiveDownloads, undefined).

%% handle_links(Account, Links) ->
%% 	case download_lib:save_downloads(Account:premium(), Links, []) of
%% 		{ok, SavedDownloads} ->
%% 			
%% 		{error, Errors} ->
%% 			
%% 	end;