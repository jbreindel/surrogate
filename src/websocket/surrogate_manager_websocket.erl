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

handle_join(ServiceURL, WebSocket, State) -> 
	{noreply, []}.

handle_incoming(ServiceURL, WebSocket, Message, State) ->
	{noreply, []}.

handle_broadcast(Message, State) ->
	{noreply, []}.

handle_info(Info, State) ->
	{noreply, NewState}.

handle_close(Reason, ServiceURL, WebSocket, State) ->
	{noreply, []}.

terminate(Reason, State) -> 
	ok. 