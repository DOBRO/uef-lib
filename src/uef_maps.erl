-module(uef_maps).

-export([find_nested/2]).


%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% find_nested/3
-spec find_nested(Keys :: [], map()) -> {ok, term()} | error.
find_nested(Keys, Map) ->
	find_nested(Keys, Map, error).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% find_nested/3
-spec find_nested(Keys :: [], map(), Result) -> Result when Result :: {ok, term()} | error.
find_nested([], _, Result) ->
	Result;
find_nested([Key|Tail], Map, _) ->
	case maps:find(Key, Map) of
		{ok, NestedMap} = Result ->
			find_nested(Tail, NestedMap, Result);
		_ ->
			error
	end.
