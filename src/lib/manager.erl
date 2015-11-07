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
-export([pid_name/1, start/1, loop/1]).
-include("download_status.hrl").

pid_name(Account) ->
	list_to_atom(Account:id() ++ "-manager").
	
alive(Account) ->
	ManagerName = pid_name(Account),
	case whereis(ManagerName) of 
		undefined ->
			false;
		Pid ->
			Pid
	end.

start(Account) ->
	erlang:display({manager_start, Account}),
	case alive(Account) of 
		false ->
			erlang:process_flag(trap_exit, true),
			ManagerPid = erlang:spawn_link(?MODULE, loop, [Account]),
			receive
		        {'EXIT', ManagerPid, normal} -> % not a crash
		            {noreply, undefined};
		        {'EXIT', ManagerPid, shutdown} -> % manual shutdown, not a crash
		            {noreply, undefined};
		        {'EXIT', ManagerPid, Reason} ->
		            erlang:display({manager_reason, Reason})
    		end;
		Pid ->
			erlang:display({manager_pid, Pid}),
			{noreply, undefined}
	end.

%%----------------------------------------------------------------------
%% Function: notify_subscriber/2
%% Purpose: Sends the subscriber the data if it exists
%% Args:   	Subscriber - Process monitoring events
%%			Data - data to send to the subscriber
%%----------------------------------------------------------------------
notify_subscriber(Subscriber, Data) ->
	case is_pid(Subscriber) of
		true ->
			Subscriber ! Data;
		false ->
			false
	end.

%% next_accquired_download(Downloads, NumDownloads) when Downloads:size() >= NumDownloads ->
%% 	ok.
%% next_accquired_download(Downloads, NumDownloads) when Downloads:size() < NumDownloads ->
%% 	case boss_db:find(download, [{status, equals, ?DL_ACQUIRED}], [{order_by, created_time}]) of
%% 		[] ->
%% 			undefined;
%% 		[Download|Downloads] ->
%% 			Download
%% 	end.
%% 
%% execute(Downloads) ->
%% 	case boss_db:find_first(config) of
%% 		undefined ->
%% 			ok;
%% 		Config ->
%% 			NumDownloads = Config:num_simultaneous_downloads(),
%% 			case next_accquired_download(Downloads, NumDownloads) of
%% 				ok ->
%% 					ok;
%% 				undefined ->
%% 					ok;
%% 				Download ->
%% 					%% spawn execution process
%% 					ok
%% 			end
%% 	end.

add_downloads(Dict, []) ->
	Dict;
add_downloads(Dict, [Download|Downloads]) ->
	dict:store(Download:id(), [{download, Download}], Dict).

login_premiums(Account, RefreshedAccount) ->
	case RefreshedAccount:first_premium() of
		undefined ->
			ok;
		RefreshedPremium ->
			case Account:first_premium() of
				undefined ->
					premium_lib:premium_login(RefreshedPremium);
				Premium ->
					RefreshedPremId = RefreshedPremium:id(),
					PremiumId = Premium:id(),
					erlang:display([{refresh_prem_id, RefreshedPremId}, {prem_id, PremiumId}]),
					case RefreshedPremId /= PremiumId of
						false ->
							premium_lib:premium_login(Premium);
						true ->
							ok
					end;
				true ->
					ok
			end
	end.

%%----------------------------------------------------------------------
%% Function: loop/1
%% Purpose: Loops the manager with an Account and default options
%% Args:   	Account - account cb record
%%			Downloads - OrderdDictionary of current downloads by id
%%			Subscriber - Process monitoring events
%%----------------------------------------------------------------------
loop(Account) ->
	register(pid_name(Account), self()),
	case Account:first_premium() of
		undefined ->
			ok;
		Premium ->
			premium_lib:premium_login(Premium)
	end,
	loop(Account, dict:new(), undefined).

%%----------------------------------------------------------------------
%% Function: loop/3
%% Purpose: Loops the manager with an Account, Downloads, and Subscriber
%% Args:   	Account - account cb record
%%			Downloads - OrderdDictionary of current downloads by id
%%			Subscriber - Process monitoring events
%%----------------------------------------------------------------------
loop(Account, Downloads, Subscriber) ->
	receive

		%%%%%%%%%%%%%%%%%%%%%
		%% Client Messages %%
		%%%%%%%%%%%%%%%%%%%%%

		%%
		% subscriber connects to manager
		%%
		{subscriber_connect, SubscriberPid} ->
			erlang:display({subscriber_connect, SubscriberPid}),
			case boss_db:find(Account:id()) of
				undefined ->
					error;
				RefreshedAccount ->
					login_premiums(Account, RefreshedAccount)
			end,
			notify_subscriber(SubscriberPid, {manager_downloads, dict:to_list(Downloads)}),
			loop(Account, Downloads, SubscriberPid);

		%%
		% subscriber sent links to the manager
		%%
		{subscriber_downloads, DownloadLinkArray} ->
			erlang:display({manager_downloads_account, Account}),
			case download_lib:save_downloads(Account:first_premium(), DownloadLinkArray) of
				{ok, SavedDownloads} ->
					erlang:spawn(acquirer, acquire_downloads, [Account, SavedDownloads]),
					notify_subscriber(Subscriber, {manager_downloads_saved, SavedDownloads}),
					loop(Account, add_downloads(Downloads, SavedDownloads), Subscriber);
				{error, Error} ->
					erlang:display({manager_downloads_error, Error}),
					notify_subscriber(Subscriber, {manager_downloads_error, Error}),
					loop(Account, Downloads, Subscriber)
			end;
		
		%%
		% called when the subscriber wants to refresh their downloads
		%%
		{subscriber_refresh, _} ->
			notify_subscriber(Subscriber, {manager_downloads, dict:to_list(Downloads)}),
			loop(Account, Downloads, Subscriber);

		%%
		% subscriber no longer is connected
		%%
		{subscriber_disconnect, _} ->
			erlang:display({subscriber_disconnect, undefined}),
			loop(Account, Downloads, undefined);
			
		%%%%%%%%%%%%%%%%%%%%%%%
		%% Download Messages %%
		%%%%%%%%%%%%%%%%%%%%%%%
		
		%%
		% download is not found
		%%
		{download_not_found, Download} ->
			UpdatedDownload = Download:set(status, ?DL_NOT_FOUND),
			case UpdatedDownload:save() of
				{ok, SavedDownload} ->
					notify_subscriber(Subscriber, {manager_on_download_not_found, [{download, Download}]});
				{error, Errors} ->
					notify_subscriber(Subscriber, {manager_on_download_error, [{download, Download}, {errors, Errors}]})
			end,
			loop(Account, Downloads, Subscriber);
		
		%%
		% download has been accquired
		%%
		{download_accquired, Download} ->
			UpdatedDownload = Download:set(status, ?DL_ACQUIRED),
			case UpdatedDownload:save() of
				{ok, SavedDownload} ->
					notify_subscriber(Subscriber, {manager_on_download_accquired, [{download, Download}]});
				{error, Errors} ->
					notify_subscriber(Subscriber, {manager_on_download_error, [{download, Download}, {errors, Errors}]})
			end,
			loop(Account, Downloads, Subscriber);
			
		%%
		% download has started
		%%
		{download_started, Download} ->
			UpdatedDownload = Download:set(status, ?DL_ACTIVE),
			case UpdatedDownload:save() of
				{ok, SavedDownload} ->
					notify_subscriber(Subscriber, {manager_on_download_started, [{download, Download}]});
				{error, Errors} ->
					notify_subscriber(Subscriber, {manager_on_download_error, [{download, Download}, {errors, Errors}]})
			end,
			loop(Account, Downloads, Subscriber);
			
		%%
		% download has finished
		%%
		{download_complete, Download} ->
			UpdatedDownload = Download:set(status, ?DL_COMPLETED),
			case UpdatedDownload:save() of
				{ok, SavedDownload} ->
					notify_subscriber(Subscriber, {manager_on_download_complete, [{download, Download}]});
				{error, Errors} ->
					notify_subscriber(Subscriber, {manager_on_download_error, [{download, Download}, {errors, Errors}]})
			end,
			loop(Account, Downloads, Subscriber);
	
	Message ->
			erlang:display({message, Message})
			
	end.