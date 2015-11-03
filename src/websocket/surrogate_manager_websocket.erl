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
	SubscriberName = subscriber:pid_name(Account),
	case whereis(SubscriberName) of 
		undefined ->
			false;
		Pid ->
			Pid
	end.

subscriber_start(Account, WebSocket) ->
	case subscriber_alive(Account) of 
		false ->
			spawn_link(subscriber, loop, [Account, WebSocket]),
			receive
		        {'EXIT', Pid, normal} -> % not a crash
		            {noreply, undefined};
		        {'EXIT', Pid, shutdown} -> % manual shutdown, not a crash
		            {noreply, undefined};
		        {'EXIT', Pid, _} ->
		            subscriber_start(Account, WebSocket)
    		end;
		Pid ->
			{noreply, undefined}
	end.

subscriber_message(Account, Message) ->
	case subscriber_alive(Account) of
		false ->
			false;
		Pid ->
			{struct, MessageJson} = mochijson:decode(binary_to_list(Message)),
			erlang:display({message, MessageJson})
	end.

parse_cookies([], CookieProps) ->
	CookieProps;
parse_cookies([Cookie|Cookies], CookieProps) ->
	case string:tokens(Cookie, "=") of
		[CookieName|CookieValue] ->
			parse_cookies(Cookies, CookieProps ++ {CookieName, CookieValue});
		_ ->
			parse_cookies(Cookies, CookieProps)
	end.

http_cookie(Name, Req) ->	
	{Headers, HttpReq} = cowboy_req:headers(Req),
	case proplists:is_defined(<<"cookie">>, Headers) of
		true ->
			BinCookies = proplists:get_value(<<"cookie">>, Headers),
			CookieStr = binary_to_list(BinCookies),
			Cookies = string:tokens(CookieStr, ";"),
			parse_cookies(Cookies, []);
		false ->
			erlang:display(Headers)
	end.
		
handle_join(ServiceUrl, WebSocket, State) ->
	#state{users = Users} = State,
	erlang:display(http_cookie(<<"cookie">>, Req)),
	case Req:cookie("account_id") of
		undefined -> 
			{noreply, State};
		Id ->
			case boss_db:find(Id) of
                undefined -> 
					{noreply, State};
                Account ->
					case Account:session_identifier() =:= Req:cookie("session_id") of
						true ->
							spawn(?MODULE, subscriber_start, [Account, WebSocket]),
							{noreply, #state{users = dict:store(WebSocket, [{account, Account}], Users)}};
						false ->
							{noreply, State}
					end
			end
	end.

handle_incoming(ServiceUrl, WebSocket, Message, State) ->
	#state{users = Users} = State,
	case Users:find(WebSocket) of
		{ok, AccountProps} ->
			case proplists:is_defined(account, AccountProps) of
				true ->
					Account = proplists:get_value(account, AccountProps),
					subscriber_message(Account, Message),
					{noreply, State};
				false ->
					{noreply, State}
			end;
		error ->
			erlang:display({websocket, WebSocket}),
			{noreply, State}
	end.

handle_broadcast(Message, State) ->
	erlang:display({message, Message}),
	{noreply, State}.

handle_info(Info, State) ->
	erlang:display({handle_info, Info}),
	{noreply, State}.

handle_close(Reason, ServiceURL, WebSocket, State) ->
	#state{users = Users} = State,
	{noreply, #state{users = dict:erase(WebSocket, Users)}}.

terminate(Reason, State) ->
	erlang:display("terminate"),
	ok.