

-module(surrogate_landing_controller, [Req]).
-compile(export_all).

landing('GET', []) ->
	{output, "Hello, Dominique!"}.