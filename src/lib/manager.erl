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
-export([pid_name/1, alive/1, start/1, loop/1]).
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

notify_subscriber(Subscriber, Data) ->
	case is_pid(Subscriber) of
		true ->
			Subscriber ! Data;
		false ->
			false
	end.

num_active_downloads() ->
	case boss_db:find(download, [{status, equals, ?DL_ACTIVE}], [{order_by, created_time}]) of
		[] ->
			0;
		ActiveDownloads ->
			length(ActiveDownloads)
	end.

next_acquired_download() ->
	case boss_db:find(download, [{status, equals, ?DL_ACQUIRED}], [{order_by, created_time}]) of
		[] ->
			undefined;
		[Download|Downloads] ->
			Download
	end.

schedule_downloads(Account, NumSimul, NumActive, DownloadsScheduled) when NumActive >= NumSimul ->
	DownloadsScheduled;
schedule_downloads(Account, NumSimul, NumActive, DownloadsScheduled) when NumActive < NumSimul ->
	case next_acquired_download() of
		undefined ->
			[];
		Download ->
			ActiveDownload = Download:set(status, ?DL_ACTIVE),
			case ActiveDownload:save() of
				{ok, SavedDownload} ->
					spawn(downloader, execute, [Account, ActiveDownload]),
					schedule_downloads(Account, NumSimul, NumActive + 1, DownloadsScheduled ++ [SavedDownload]);
				{error, Errors} ->
					erlang:display({error, Errors})
			end
	end.

schedule(Account, DownloadsDict) ->
	case boss_db:find_first(config) of
		undefined ->
			{error, "Cannot find Config."};
		Config ->
			NumSimul = Config:num_simultaneous_downloads(),
			case num_active_downloads() of
				NumActive when NumActive < NumSimul ->
					ScheduledDownloads = schedule_downloads(Account, NumSimul, NumActive, []),
					add_downloads(DownloadsDict, ScheduledDownloads);
				NumActive ->
					DownloadsDict
			end
	end.

add_downloads(Dict, []) ->
	Dict;
add_downloads(Dict, [Download|Downloads]) ->
	UpdatedDict = dict:store(Download:id(), [{download, Download}], Dict),
	add_downloads(UpdatedDict, Downloads).

login_premiums(Account, RefreshedAccount) ->
	case RefreshedAccount:first_premium() of
		undefined ->
			ok;
		RefreshedPremium ->
			case Account:first_premium() of
				undefined ->
					premium_lib:premium_login(RefreshedAccount, RefreshedPremium);
				Premium ->
					RefreshedPremId = RefreshedPremium:id(),
					PremiumId = Premium:id(),
					erlang:display([{refresh_prem_id, RefreshedPremId}, {prem_id, PremiumId}]),
					premium_lib:premium_login(RefreshedAccount, RefreshedPremium);
				true ->
					ok
			end
	end.

download_not_found(Subscriber, Download) ->
	UpdatedDownload = Download:set(status, ?DL_NOT_FOUND),
	case UpdatedDownload:save() of
		{ok, SavedDownload} ->
			notify_subscriber(Subscriber, {manager_download_not_found, [{download, Download}]});
		{error, Errors} ->
			notify_subscriber(Subscriber, {manager_download_error, [{download, Download}, {errors, Errors}]})
	end.

download_acquire(Subscriber, Download, RealUrl) ->
	AcquiredDownload = Download:set([{status, ?DL_ACQUIRED}, {real_url, RealUrl}]),
	case AcquiredDownload:save() of
		{ok, SavedAcquiredDownload} ->
			erlang:display({manager_download_acquired, [{download, SavedAcquiredDownload}]}),
			notify_subscriber(Subscriber, {manager_download_acquired, [{download, SavedAcquiredDownload}]});
		{error, Errors} ->
			notify_subscriber(Subscriber, {manager_download_error, [{download, Download}, {errors, Errors}]})
	end.

download_start(Subscriber, Download) ->
	UpdatedDownload = Download:set(status, ?DL_ACTIVE),
	case UpdatedDownload:save() of
		{ok, SavedDownload} ->
			notify_subscriber(Subscriber, {manager_download_started, [{download, Download}]});
		{error, Errors} ->
			notify_subscriber(Subscriber, {manager_download_error, [{download, Download}, {errors, Errors}]})
	end.

download_complete(Subscriber, Download) ->
	UpdatedDownload = Download:set(status, ?DL_COMPLETED),
	case UpdatedDownload:save() of
		{ok, SavedDownload} ->
			notify_subscriber(Subscriber, {manager_download_complete, [{download, SavedDownload}]});
		{error, Errors} ->
			notify_subscriber(Subscriber, {manager_download_error, [{download, Download}, {errors, Errors}]})
	end.

download_fail(Subscriber, Download, Error) ->
	UpdatedDownload = Download:set(status, ?DL_FAILED),
	case UpdatedDownload:save() of
		{ok, SavedDownload} ->
			notify_subscriber(Subscriber, {manager_download_error, [{download, UpdatedDownload}, {error, Error}]});
		{error, Errors} ->
			notify_subscriber(Subscriber, {manager_download_error, [{download, Download}, {error, Error}]})
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
			premium_lib:premium_login(Account, Premium)
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
					SubscriberDownloads = lists:map(fun(SavedDownload) -> 
														{SavedDownload:id(), [{download, SavedDownload}]} 
													end, 
													SavedDownloads),
					notify_subscriber(Subscriber, {manager_downloads_saved, SubscriberDownloads}),
					loop(Account, Downloads, Subscriber);
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
		{download_not_found, DownloadProps} ->
			[{download, Download}] = DownloadProps,
			download_not_found(Subscriber, Download),
			ScheduledDownloads = schedule(Account, dict:erase(Download:id(), Downloads)),
			loop(Account, ScheduledDownloads, Subscriber);
		
		%%
		% download has been accquired
		%%
		{download_acquired, DownloadProps} ->
			[{download, Download}, {real_url, RealUrl}] = DownloadProps,
			download_acquire(Subscriber, Download, RealUrl),
			ScheduledDownloads = schedule(Account, Downloads),
			loop(Account, ScheduledDownloads, Subscriber);
			
		%%
		% download has started
		%%
		{download_started, DownloadProps} ->
			Download = proplists:get_value(download, DownloadProps),
			download_start(Subscriber, Download),
			loop(Account, Downloads, Subscriber);
		
		%%
		% download progress update
		%%
		{download_progress, DownloadProps} ->
			notify_subscriber(Subscriber, {manager_download_progress, DownloadProps}),
			Download = proplists:get_value(download, DownloadProps),
			case Download:save() of
				{ok, SavedDownload} ->
					UpdatedDownloadProps = proplists:delete(download, DownloadProps) ++ [{download, SavedDownload}],
					loop(Account, dict:store(SavedDownload:id(), UpdatedDownloadProps, Downloads), Subscriber);
				{error, Errors} ->
					loop(Account, dict:store(Download:id(), DownloadProps, Downloads), Subscriber)
			end;
					
		%%
		% download has finished
		%%
		{download_complete, DownloadProps} ->
			Download = proplists:get_value(download, DownloadProps),
			case download_lib:download_completed(Download) of
				true ->
					download_complete(Subscriber, Download);
				false ->
					download_fail(Subscriber, Download, "Stream ended prematurtly.")
			end,
			ScheduledDownloads = schedule(Account, dict:erase(Download:id(), Downloads)),
			loop(Account, ScheduledDownloads, Subscriber);
		
		%%
		% download has errored
		%%
		{download_error, DownloadProps} ->
			Download = proplists:get_value(download, DownloadProps),
			Error = proplists:get_value(error, DownloadProps),
			download_fail(Subscriber, Download, Error),
			ScheduledDownloads = schedule(Account, dict:erase(Download:id(), Downloads)),
			loop(Account, ScheduledDownloads, Subscriber);
	
		Message ->
			erlang:display({message, Message}),
			loop(Account, Downloads, Subscriber)
			
	end.