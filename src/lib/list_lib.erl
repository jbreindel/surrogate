-module(list_lib).
-compile(export_all).

list_min([H|T]) -> 
	list_min(H, T).

list_min(X, []) -> 
	X;
list_min(X, [H|T]) -> 
	list_min(erlang:min(H, X), T).

list_max([H|T]) -> 
	list_max(H, T).

list_max(X, []) -> 
	X;
list_max(X, [H|T]) -> 
	list_max(erlang:max(H, X), T).
