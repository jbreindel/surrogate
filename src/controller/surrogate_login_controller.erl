%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: surrogate_login_controller.erl
%
% AUTHOR: Jake Breindel
% DATE: 5-21-15
%
% DESCRIPTION:
%
% Controller for the login page.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(surrogate_login_controller, [Req]).
-compile(export_all).

start_manager(Account) ->
	ManagerName = manager:name(Account),
	case whereis(ManagerName) of
		undefined ->
			spawn(manager, loop, [Account]);
		Pid ->
			true
	end.

login('GET', []) ->
	case Req:cookie("account_id") of
		undefined -> 
			{ok, []};
		Id ->
			case boss_db:find(Id) of
                undefined -> 
					{redirect, "/"};
                Account ->
					case Account:session_identifier() =:= Req:cookie("session_id") of
						false ->
							{ok, [{account, Account}]};
						true ->
							{redirect, "/home/home"}
					end
			end
	end;

login('POST', []) ->
    UserName = Req:post_param("userName"),
	case boss_db:find(account, [{user_name, UserName}], [{limit,1}]) of
		[Account] ->
			case Account:check_password(Req:post_param("password")) of
                true ->
					start_manager(Account),
                    {redirect, proplists:get_value("redirect", Req:post_params(), "/home/home"), Account:login_cookies()};
                false ->
                    {ok, [{error, "Bad name/password combination"}]}
            end;
		[] ->
            {ok, [{error, [{userNameError, "No Account with username  " ++ UserName}]}]}
    end.
			