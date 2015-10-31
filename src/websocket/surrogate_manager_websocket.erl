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
-compile(export_all).

init() -> 
	{ok, []}.

handle_join(ServiceUrl, WebSocket, State) ->
	erlang:display("handle_join"),
	{noreply, []}.

handle_incoming(ServiceUrl, WebSocket, Message, State) ->
	erlang:display("handle_incoming"),
	{noreply, []}.

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