-module(surrogate_custom_tags).
-compile(export_all).

% put custom tags in here, e.g.
%
% reverse(Variables, Options) ->
%     lists:reverse(binary_to_list(proplists:get_value(string, Variables))).
%
% {% reverse string="hello" %} => "olleh"
%
% Variables are the passed-in vars in your template

percentage(Variables, Options) ->
	case proplists:get_value(download, Variables) of
		undefined ->
			"";
		Download ->
			case Download:progress() of
				0 ->
					"0";
				Progress ->
					erlang:display({Download:progress(), Download:length()}),
					Percentage = (Download:progress() / Download:length()) * 100,
					io_lib:format("~.1f", [Percentage])
			end
	end.

download_id(Variables, Options) ->
	case proplists:get_value(download, Variables) of
		undefined ->
			"";
		Download ->
			string:substr(Download:id(), 8)
	end.