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

loop(Account, Downloads, Subscriber) ->
	recieve

		%%%%%%%%%%%%%%%%%%%%%
		%% Client Messages %%
		%%%%%%%%%%%%%%%%%%%%%

		{subscriber_connect, Subscriber} ->
			loop(Account, Downloads, Subscriber);

		{links, Links} ->
			handle_links(Account, Links),
			loop(Account, Downloads, Subscriber)

		{subscriber_disconnect, _} ->
			loop(Account, Downloads, undefined).

%% handle_links(Account, Links) ->
%% 	case download_lib:save_downloads(Account:premium(), Links, []) of
%% 		{ok, SavedDownloads} ->
%% 			
%% 		{error, Errors} ->
%% 			
%% 	end;