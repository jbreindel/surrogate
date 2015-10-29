%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: 	config.erl
%
% AUTHOR: 	Jake Breindel
% DATE: 	5-20-15
%
% DESCRIPTION:
%
% Model class for config
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(config, [Id, DownloadDirectory::string(), NumSimultaneousDownloads::integer()]).
-compile(export_all).

validation_tests() -> 
	[{fun() -> 
			  FileName = DownloadDirectory ++ "foo",
			  case file:write_file(FileName, io_lib:fwrite("~p.\n", ["Hello World!"])) of
				  ok ->
					  file:delete(FileName),
					  true;
				  {error, Reason} ->
					  erlang:display(Reason),
					  false
			  end
	  end, "Download Directory is not writable"}].