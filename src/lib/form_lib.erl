%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: form_lib.erl
%
% AUTHOR: Jake Breindel
% DATE: 10-29-15
%
% DESCRIPTION:
%
% Validates form posts.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(form_lib).
-export([str_len_validator/3, validate/2]).

%%----------------------------------------------------------------------
%% Function: str_len_validator/3
%% Purpose: Performs validation on the string length.
%% Args:   	String - string to be validated
%%			Min - minimum value and message
%%			Max - maximum value and message
%%----------------------------------------------------------------------
str_len_validator(String, [{min, Min}, {message, MinMessage}], [{max, Max}, {message, MaxMessage}]) ->
	case string:len(String) of
		Len when Len < Min ->
			MinMessage;
		Len  when Len >= Max ->
			MaxMessage
		end.

validate_rule(Req, [{name, Name}, {input, Input}], Rule, Errors) ->
	case Rule(Input) of
		ErrorMessage ->
			Errors ++ [{name, Name}, {error, ErrorMessage}];
		false ->
			Errors
	end.

perform_validation_rule(Req, [{name, Name}, {rule, Rule}], Errors) ->
	case Req:post_param(Name) of
		Input ->
			validate_rule(Req, [{name, Name}, {input, Input}], Rule, Errors);
		undefined ->
			Errors ++ [{name, Name}, {error, "Need to fill out required Field"}]
	end.

%%----------------------------------------------------------------------
%% Function: validate/2
%% Purpose: Performs validation on the request given the rules and errors.
%% Args:   	Req - request
%%			Validation - validation rules for the request
%%----------------------------------------------------------------------
validate(Req, Validation) ->
	validate(Req, Validation, []).

%%----------------------------------------------------------------------
%% Function: validate/3
%% Purpose: Performs validation on the request given the rules and errors.
%% Args:   	Req - request
%%			Validation - validation rules for the request
%%			Errors - error list
%%----------------------------------------------------------------------
validate(Req, [], Errors) ->
	Errors;
validate(Req, [Validator|Validation], Errors) ->
	perform_validation_rule(Req, Validator, Errors),
	validate(Req, Validation, Errors).
	
					
	
	