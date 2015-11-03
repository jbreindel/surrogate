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
	{ok, #state{users = dict:new()}}.

subscriber_alive(Account) ->
	SubscriberName = subscriber:name(Account),
	case whereis(SubscriberName) of 
		undefined ->
			false;
		Pid ->
			Pid
	end.

start_subscriber(Account, WebSocket) ->
	case subscriber_alive(Account) of 
		false ->
			spawn_link(subscriber, loop, [Account, WebSocket]),
			receive
		        {'EXIT', Pid, normal} -> % not a crash
		            ok;
		        {'EXIT', Pid, shutdown} -> % manual shutdown, not a crash
		            ok;
		        {'EXIT', Pid, _} ->
		            start_subscriber(Account, WebSocket)
    		end;
		Pid ->
			ok
	end.
		
handle_join(ServiceUrl, WebSocket, State) ->
	#state{users = Users} = State,
	case Req:cookie("account_id") of
		undefined -> 
			{noreply, []};
		Id ->
			case boss_db:find(Id) of
                undefined -> 
					{noreply, []};
                Account ->
					case Account:session_identifier() =:= Req:cookie("session_id") of
						true ->
							spawn(?MODULE, start_subscriber, [Account, WebSocket]),
							{noreply, #state{users = dict:store(WebSocket, [{account, Account}], Users)}};
						false ->
							{noreply, []}
					end
			end
	end.

handle_incoming(ServiceUrl, WebSocket, Message, State) ->
	{struct, MessageJson} = mochijson:decode(binary_to_list(Message)),
	erlang:display({message, MessageJson}),
	{noreply, []}.

handle_broadcast(Message, State) ->
	erlang:display({message, Message}),
	{noreply, []}.

handle_info(Info, State) ->
	erlang:display({handle_info, Info}),
	{noreply, []}.

handle_close(Reason, ServiceURL, WebSocket, State) ->
	#state{users = Users} = State,
	{noreply, #state{users = dict:erase(WebSocket, Users)}}.

terminate(Reason, State) ->
	erlang:display("terminate"),
	ok. 