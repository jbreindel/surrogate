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
-export([start/2, acquire/2, acquire_downloads/2]).
-include("download_status.hrl").

notify_manager(Account, Data) ->
	case manager:alive(Account) of
		false ->
			false;
		Pid ->
			Pid ! Data
	end.

start(Account, Downloads) ->
	erlang:display({start, Downloads}),
	erlang:process_flag(trap_exit, true),
	AcquirerPid = erlang:spawn_link(?MODULE, acquire_downloads, [Account, Downloads]),
	receive
		{'EXIT', AcquirerPid, normal} -> % not a crash
			{noreply, undefined};
		{'EXIT', AcquirerPid, shutdown} -> % manual shutdown, not a crash
			{noreply, undefined};
		{'EXIT', AcquirerPid, _} ->
			start(Account, Downloads)
	end.

acquire(Account, Download) ->
	case httpc:request(Download:display_url()) of
		{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} ->
			erlang:display(binary_to_list(Body));
		_ ->
			error
	end.

acquire_downloads(Account, []) ->
	ok;
acquire_downloads(Account, [Download|Downloads]) ->
	case Download:status() of
		?DL_PENDING ->
			erlang:spawn(?MODULE, acquire, [Account, Download]);
		_ ->
			ok
	end,
	acquire_downloads(Account, Downloads).