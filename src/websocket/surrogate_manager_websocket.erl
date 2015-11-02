%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: surrogate_manager_websocket.erl
%
% AUTHOR: Jake Breindel
% DATE: 
%
% DESCRIPTION:
%
% Websocket controller for download events.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(surrogate_manager_websocket, [Req, SessionId]).
-behaviour(boss_service_handler).

-record(state,{users}).

-export([init/0, 
	handle_incoming/4, 
	handle_join/3,
        handle_broadcast/2,
	handle_close/4, 
	handle_info/2,
	terminate/2]).

init() -> 
	{ok, []}.

handle_join(ServiceUrl, WebSocket, State) ->
	{noreply, []}.

handle_incoming(ServiceUrl, WebSocket, Message, State) ->
	Downloads = mochijson:decode(binary_to_list(Message)),
	erlang:display({message, Downloads}),
	{noreply, []}.

handle_broadcast(Message, State) ->
	erlang:display({message, Message}),
	{noreply, []}.

handle_info(Info, State) ->
	erlang:display({handle_info, Info}),
	{noreply, []}.

handle_close(Reason, ServiceURL, WebSocket, State) ->
	erlang:display("handle_close"),
	{noreply, []}.

terminate(Reason, State) ->
	erlang:display("terminate"),
	ok. 