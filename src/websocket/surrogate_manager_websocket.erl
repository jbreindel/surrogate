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

-export([
	init/0, 
	handle_incoming/4, 
	handle_join/3,
    handle_broadcast/2,
	handle_close/4, 
	handle_info/2,
	terminate/2]).

init() -> 
	{ok, #state{users = dict:new()}}.

subscriber_message(Account, Message) ->
	case subscriber:alive(Account) of
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
		[Name|CookieValue] ->
			[Value] = CookieValue,
			ParsedName = re:replace(Name, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
			ParsedValue = re:replace(Value, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
			parse_cookies(Cookies, CookieProps ++ [{ParsedName, ParsedValue}]);
		_ ->
			parse_cookies(Cookies, CookieProps)
	end.

http_cookies(Req) ->	
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
	Cookies = http_cookies(Req),
	case proplists:get_value("account_id", Cookies) of
		undefined -> 
			{noreply, State};
		Id ->
			case boss_db:find(Id) of
                undefined -> 
					{noreply, State};
                Account ->
					case Account:session_identifier() =:= proplists:get_value("session_id", Cookies) of
						true ->
							erlang:spawn(subscriber, start, [Account, WebSocket]),
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