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
	erlang:display("Initializing Websocket!"),
	{ok, []}.

handle_join(ServiceUrl, WebSocket, State) ->
	erlang:display("handle_join"),
	{noreply, [{message, "Joined!"}]}.

handle_incoming(ServiceUrl, WebSocket, Message, State) ->
	erlang:display("handle_incoming"),
	{noreply, [{message, "Received Message!"}]}.

handle_broadcast(Message, State) ->
	erlang:display("handle_broadcast"),
	{noreply, []}.

handle_info(Info, State) ->
	erlang:display("handle_info"),
	{noreply, []}.

handle_close(Reason, ServiceURL, WebSocket, State) ->
	erlang:display("handle_close"),
	{noreply, []}.

terminate(Reason, State) ->
	erlang:display("terminate"),
	ok. 