-module(uef_maps).

-export([find_nested/2]).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type mapkey() :: term().
-type mapkeys() :: [mapkey()].
-type find_result() :: {ok, term()} | error.


%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% find_nested/3
-spec find_nested(mapkeys(), map()) -> find_result().
find_nested(Keys, Map) ->
	find_nested_unsafe(Keys, Map).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% find_nested_unsafe/2
-spec find_nested_unsafe(mapkeys(), map()) -> find_result().
find_nested_unsafe(Keys, Map) ->
	find_nested(Keys, Map, unsafe, error).

%% find_nested/4
-spec find_nested(mapkeys(), map(), safe | unsafe, find_result()) -> find_result().
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
