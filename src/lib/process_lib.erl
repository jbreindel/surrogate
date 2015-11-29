
-module(process_lib).
-compile(export_all).

cond_send(Pid, Data) ->
	case is_pid(Pid) of
		true ->
			Pid ! Data;
		false ->
			false
	end.

find_send(PidName, Data) ->
	case whereis(PidName) of 
		undefined ->
			false;
		Pid ->
			Pid ! Data
	end.
