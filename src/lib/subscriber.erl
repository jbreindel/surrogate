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
			Manager ! Data;
		false ->
			false
	end.

alive(Account) ->
	SubscriberName = pid_name(Account),
	erlang:display({account, Account}, {subscriber_name, SubscriberName}),
	case whereis(SubscriberName) of 
		undefined ->
			false;
		Pid ->
			Pid
	end.

start(Account, WebSocket) ->
	case alive(Account) of 
		false ->
			erlang:process_flag(trap_exit, true),
			SubscriberPid = erlang:spawn_link(?MODULE, loop, [Account, WebSocket]),
			receive
		        {'EXIT', SubscriberPid, normal} -> % not a crash
		            {noreply, undefined};
		        {'EXIT', SubscriberPid, shutdown} -> % manual shutdown, not a crash
		            {noreply, undefined};
		        {'EXIT', SubscriberPid, _} ->
		            start(Account, WebSocket)
    		end;
		Pid ->
			{noreply, undefined}
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
	