

-module(surrogate_home_controller, [Req]).
-compile(export_all).

landing('GET', []) ->
	{output, "Hello, Dominique!"}.