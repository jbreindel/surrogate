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

notify_manager(Manager, Data) ->
	case is_pid(Manager) of
		true ->
			erlang:display({notify_manager, Data}),
			Manager ! Data;
		false ->
			false
	end.

notify_websocket(WebSocket, Data) ->
	case is_pid(WebSocket) of
		true ->
			erlang:display({notify_websocket, Data}),
			WebSocket ! Data;
		false ->
			false
	end.

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

loop(Account, WebSocket) ->
	register(pid_name(Account), self()),
	ManagerName = manager:pid_name(Account),
	case whereis(ManagerName) of
		undefined ->
			undefined;
		Manager ->
			notify_manager(Manager, {subscriber_connect, self()}),
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
					notify_manager(Manager, {subscriber_downloads, DownloadsArray});
				{struct, [{"refresh", _}]} ->
					notify_manager(Manager, {subscriber_refresh, undefined});
				Json ->
					erlang:display(Json)
			end;
		
		{websocket_close, _} ->
			erlang:display({websocket_close, undefined}),
			notify_manager(Manager, {subscriber_disconnect, undefined}),
			exit(normal);
		
		%%%%%%%%%%%%%%%%%%%%%%
		%% Manager Messages %%
		%%%%%%%%%%%%%%%%%%%%%%

		{manager_downloads, Downloads} ->
			erlang:display({manager_downloads, Downloads}),
			%WebSocketJson = iolist_to_binary(mochijson2:encode({struct, [{downloads, {array, Downloads}}]})),
			notify_websocket(WebSocket, {text, "{}"});

		{manager_downloads_saved, Downloads} ->
			erlang:display({manager_downloads_saved, Downloads}),
			%WebSocketJson = iolist_to_binary(mochijson2:encode({struct, [{downloads, {array, Downloads}}]})),
			notify_websocket(WebSocket, {text, "{}"});

		{manager_download_acquired, Download} ->
			erlang:display({manager_download_acquired, Download});
		
		{manager_download_not_found, Download} ->
			erlang:display({manager_download_not_found, Download});
		
		{manager_download_progress, DownloadProps} ->
			notify_websocket(WebSocket, {text, "DownloadProgress"});
		
		{manager_download_error, Error} ->
			erlang:display({manager_downloads_error, Error});
		
		Message ->
			erlang:display(Message)
	end,
	loop(Account, WebSocket, Manager).
	