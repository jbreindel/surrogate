%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: subscriber.erl
%
% AUTHOR: Jake Breindel
% DATE: 11-2-15
%
% DESCRIPTION:
%
% Subscribes to events from the websocket
% and to the manager.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(subscriber).
-export([pid_name/1, alive/1, start/2, loop/2]).
-include("download_status.hrl").

pid_name(Account) ->
	list_to_atom(Account:id() ++ "-subscriber").

alive(Account) ->
	SubscriberName = pid_name(Account),
	case whereis(SubscriberName) of 
		undefined ->
			false;
		Pid ->
			Pid
	end.

start(Account, WebSocket) ->
	erlang:display({subscriber_start, WebSocket}),
	case alive(Account) of 
		false ->
			erlang:process_flag(trap_exit, true),
			SubscriberPid = erlang:spawn_link(?MODULE, loop, [Account, WebSocket]),
			receive
		        {'EXIT', SubscriberPid, normal} -> % not a crash
		            {noreply, undefined};
		        {'EXIT', SubscriberPid, shutdown} -> % manual shutdown, not a crash
		            {noreply, undefined};
		        {'EXIT', SubscriberPid, Reason} ->
		            erlang:display({reason, Reason})
    		end;
		Pid ->
			erlang:display({subscriber_pid, Pid}),
			{noreply, undefined}
	end.

serialize_downloads(Message, Downloads) ->
	DownloadPropList = lists:map(fun({DownloadId, DownloadProps}) -> DownloadProps end, Downloads),
	DownloadJsonStruct = {struct, [{message, Message}, {downloads, lists:map(fun download_lib:download_to_json/1, DownloadPropList)}]},
	JsonDownloads = iolist_to_binary(mochijson2:encode(DownloadJsonStruct)),
	binary_to_list(JsonDownloads).

serialize_download(Message, Download) ->
	DownloadJsonStruct = {struct, [{message, Message}, {download, download_lib:download_to_json(Download)}]},
	JsonDownloads = iolist_to_binary(mochijson2:encode(DownloadJsonStruct)),
	binary_to_list(JsonDownloads).

loop(Account, WebSocket) ->
	register(pid_name(Account), self()),
	ManagerName = manager:pid_name(Account),
	case whereis(ManagerName) of
		undefined ->
			undefined;
		Manager ->
			proc_lib:cond_send(Manager, {subscriber_connect, self()}),
			loop(Account, WebSocket, Manager)
	end.

loop(Account, WebSocket, Manager) ->
	receive
		
		%%%%%%%%%%%%%%%%%%%%%
		%% Client Messages %%
		%%%%%%%%%%%%%%%%%%%%%
		
		{websocket_message, Message} ->
			erlang:display({websocket_message, Message}),
			case mochijson:decode(binary_to_list(Message)) of
				{struct, [{"downloads", {array, DownloadsArray}}]} ->
					proc_lib:cond_send(Manager, {subscriber_downloads, DownloadsArray});
				{struct, [{"refresh", _}]} ->
					proc_lib:cond_send(Manager, {subscriber_refresh, undefined});
				Json ->
					erlang:display(Json)
			end;
		
		{websocket_close, _} ->
			erlang:display({websocket_close, undefined}),
			proc_lib:cond_send(Manager, {subscriber_disconnect, undefined}),
			exit(normal);
		
		%%%%%%%%%%%%%%%%%%%%%%
		%% Manager Messages %%
		%%%%%%%%%%%%%%%%%%%%%%

		{manager_downloads, Downloads} ->
			JsonDownloads = serialize_downloads(<<"downloads">>, Downloads),
			proc_lib:cond_send(WebSocket, {text, JsonDownloads});

		{manager_downloads_saved, Downloads} ->
			JsonDownloads = serialize_downloads(<<"downloads_save">>, Downloads),
			proc_lib:cond_send(WebSocket, {text, JsonDownloads});

		{manager_download_acquired, DownloadProps} ->
			JsonDownload = serialize_download(<<"download_acquired">>, DownloadProps),
			proc_lib:cond_send(WebSocket, {text, JsonDownload});
		
		{manager_download_not_found, DownloadProps} ->
			JsonDownload = serialize_download(<<"download_not_found">>, DownloadProps),
			proc_lib:cond_send(WebSocket, {text, JsonDownload});
		
		{manager_download_progress, DownloadProps} ->
			JsonDownload = serialize_download(<<"download_progress">>, DownloadProps),
			proc_lib:cond_send(WebSocket, {text, JsonDownload});
		
		{manager_download_complete, DownloadProps} ->
			JsonDownload = serialize_download(<<"download_complete">>, DownloadProps),
			proc_lib:cond_send(WebSocket, {text, JsonDownload});
		
		{manager_download_error, DownloadErrorProps} ->
			JsonDownload = serialize_download(<<"download_error">>, DownloadErrorProps),
			erlang:display({manager_downloads_error, JsonDownload});
		
		Message ->
			erlang:display(Message)
	end,
	loop(Account, WebSocket, Manager).
	