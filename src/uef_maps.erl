-module(uef_maps).

-export([find_nested/2]).


%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% find_nested/3
-spec find_nested(Keys :: [], map()) -> {ok, term()} | error.
find_nested(Keys, Map) ->
	find_nested_unsafe(Keys, Map).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

find_nested_unsafe(Keys, Map) ->
	find_nested(Keys, Map, unsafe, error).

%% find_nested/4
-spec find_nested(Keys :: [], map(), safe | unsafe, Result) -> Result when Result :: {ok, term()} | error.
find_nested([], _, _, Result) ->
	Result;
find_nested([Key|Tail], Map, Safe, _) ->
	Result = case {is_map(Map), Safe} of
		{false, safe}  -> error; % don't fail, return 'error' safely
		_ -> maps:find(Key, Map) % may fail here when {false, unsafe}
	end,
	case Result of
		{ok, NestedMap} -> find_nested(Tail, NestedMap, Safe, Result);
		error -> error
	end.
