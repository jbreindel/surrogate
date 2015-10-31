%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: surrogate_event_websocket.erl
%
% AUTHOR: Jake Breindel
% DATE: 
%
% DESCRIPTION:
%
% Websocket controller for download events.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(surrogate_event_websocket, [Req, SessionId]).
-compile(export_all).

init() -> 
	{ok, []}.