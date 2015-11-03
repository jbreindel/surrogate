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

-module(manager).
-export([pid_name/1, loop/2]).
-include("download_status.hrl").

pid_name(Account) ->
	list_to_atom(Account:id() ++ "-subscriber").

notify_manager(Manager, Data) ->
	case is_pid(Manager) of
		true ->
			Manager ! Data;
		false ->
			false
	end.

loop(Account, WebSocket) ->
	register(pid_name(Account), self()),
	ManagerName = manager:pid_name(Account),
	case whereis(ManagerName) of
		undefined ->
			error;
		Manager ->
			loop(Account, WebSocket, Manager)
	end.

loop(Account, WebSocket, Manager) ->
	receive
		{subscriber_downloads, DownloadMessage} ->
			loop(Account, WebSocket, Manager);
		{subscriber_disconnect, _} ->
			notify_manager(Manager, {subscriber_disconnect, undefined});
		_ ->
			loop(Account, WebSocket, Manager)
	end.
	