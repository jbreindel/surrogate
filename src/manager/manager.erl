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

%%----------------------------------------------------------------------
%% Function: loop/1
%% Purpose: Loops the manager with an Account and default options
%% Args:   Req - Account
%%----------------------------------------------------------------------
loop(Account) ->
	loop(Account, [], undefined);

%%----------------------------------------------------------------------
%% Function: loop/3
%% Purpose: Loops the manager with an Account, Downloads, and Subscriber
%% Args:   Req - Account
%%----------------------------------------------------------------------
loop(Account, Downloads, Subscriber) ->
	recieve

		%%%%%%%%%%%%%%%%%%%%%
		%% Client Messages %%
		%%%%%%%%%%%%%%%%%%%%%

		%%
		% subscriber connects to manager
		%%
		{subscriber_connect, Subscriber} ->
			Subscriber ! {downloads Downloads},
			loop(Account, Downloads, Subscriber);

		%%
		% subscriber sent links to the manager
		%%
		{links, Links} ->
			handle_links(Account, Links),
			loop(Account, Downloads, Subscriber);
		
		%%
		% called when the subscriber wants to refresh their downloads
		%%
		{subscriber_refresh, Subscriber} ->
			Subscriber ! {downloads, Downloads},
			loop(Account, Downloads, Subscriber);

		%%
		% subscriber no longer is connected
		%%
		{subscriber_disconnect, _} ->
			loop(Account, Downloads, undefined);
			
		%%%%%%%%%%%%%%%%%%%%%%%
		%% Download Messages %%
		%%%%%%%%%%%%%%%%%%%%%%%
		
		%%
		% download is not found
		%%
		{download_not_found, Download} ->
			case is_pid(Subscriber) of
				true ->
					UpdatedDownload = Download::set(status, ?DL_NOT_FOUND),
					case UpdatedDownload::save() of
						{ok, SavedDownload} ->
            				Subscriber ! {on_download_not_found, Download};
						{error, Errors} ->
				   			{ok, [{errors, Errors}]}
					end
			end;
		
		%%
		% download has been accquired
		%%
		{download_accquired, Download} ->
			case is_pid(Subscriber) of
				true ->
					UpdatedDownload = Download::set(status, ?DL_PENDING),
					case UpdatedDownload::save() of
						{ok, SavedDownload} ->
            				Subscriber ! {on_download_accquired, Download};
						{error, Errors} ->
				   			{ok, [{errors, Errors}]}
					end
			end;
			
		%%
		% download has started
		%%
		{download_started, Download} ->
			case is_pid(Subscriber) of
				true ->
					UpdatedDownload = Download::set(status, ?DL_ACTIVE),
					case UpdatedDownload::save() of
						{ok, SavedDownload} ->
            				Subscriber ! {on_download_started, Download};
						{error, Errors} ->
				   			{ok, [{errors, Errors}]}
					end
			end
			
	end.

%% handle_links(Account, Links) ->
%% 	case download_lib:save_downloads(Account:premium(), Links, []) of
%% 		{ok, SavedDownloads} ->
%% 			
%% 		{error, Errors} ->
%% 			
%% 	end;