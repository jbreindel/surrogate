%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: acquirer.erl
%
% AUTHOR: Jake Breindel
% DATE: 6-4-15
%
% DESCRIPTION:
%
% Acquirer that acquires actual download
% links.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(acquirer).
-export([acquire/3]).

acquire(Manager, Premium, Download) ->
	ok.
	%%http:request(get, {Download:get_display_url(), [{"User-Agent", UA}]}, [], []).