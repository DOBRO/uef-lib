-module(uef_maps).

-export([find_nested/2]).
-export([get_nested/2, get_nested/3]).
-export([new_nested/1, new_nested/2]).

%%%------------------------------------------------------------------------------
%%%   EUnit
%%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type mapkey() :: term().
-type mapkeys() :: [mapkey()].
-type find_result() :: {ok, term()} | error.


%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% find_nested/2
-spec find_nested(mapkeys(), map()) -> find_result().
find_nested(Keys, Map) ->
	find_nested_unsafe(Keys, Map).


%% get_nested/2
-spec get_nested(mapkeys(), map()) -> term().
get_nested(Keys, Map) ->
	FindResult = find_nested_unsafe(Keys, Map), % may fail here with a {badmap,Map} exception
	case FindResult of
		{ok, Value} -> Value;
		error -> erlang:error({badkeys,Keys},[Keys,Map])
	end.

%% get_nested/3
-spec get_nested(mapkeys(), map(), Default :: term()) -> term().
get_nested(Keys, Map, Default) when is_map(Map) ->
	FindResult = find_nested_safe(Keys, Map),
	case FindResult of
		{ok, Value} -> Value;
		error -> Default
	end;
get_nested(Keys, Map, Default) ->
	erlang:error({badmap,Map},[Keys, Map, Default]).

%% new_nested/1
-spec new_nested(mapkeys()) -> map().
new_nested(Keys) ->
	new_nested(Keys, #{}).

%% new_nested/2
-spec new_nested(mapkeys(), Value :: term()) -> map().
new_nested([], _) ->
	#{};
new_nested([Key], Value) ->
	#{Key => Value};
new_nested(Keys, Value) when is_list(Keys) ->
	[LastKey | Rest] = lists:reverse(Keys),
	new_nested_1(Rest, #{LastKey => Value});
new_nested(Keys, Value) ->
	erlang:error({badlist, Keys}, [Keys, Value]).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% find_nested_unsafe/2
-spec find_nested_unsafe(mapkeys(), map()) -> find_result().
find_nested_unsafe(Keys, Map) ->
	find_nested(Keys, Map, unsafe, error).

%% find_nested_safe/2
-spec find_nested_safe(mapkeys(), map()) -> find_result().
find_nested_safe(Keys, Map) ->
	find_nested(Keys, Map, safe, error).

%% find_nested/4
-spec find_nested(mapkeys(), map(), safe | unsafe, find_result()) -> find_result().
find_nested([], _, _, Result) ->
	Result;
find_nested([Key|Tail], Map, Safe, _) ->
	Result = case {is_map(Map), Safe} of
		{false, safe}  -> error; % don't fail, return 'error' safely
		_ -> maps:find(Key, Map) % may fail here (when {false, unsafe}) with a {badmap,Map} exception
	end,
	case Result of
		{ok, NestedMap} -> find_nested(Tail, NestedMap, Safe, Result);
		error -> error
	end;
find_nested(NotList, _, _, _) ->
	erlang:error({badlist, NotList}).

%% new_nested_1/2
-spec new_nested_1(mapkeys(), map()) -> map().
new_nested_1([], Map) -> Map;
new_nested_1([Key|Tail], Map) ->
	new_nested_1(Tail, #{Key => Map}).

%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

-ifdef(TEST).

find_nested__and__get_nested_test_() ->
	Value = value,
	M3 = #{3 => Value},
	M2 = #{2 => M3},
	M1 = #{1 => M2},
	M0 = #{0 => M1},
	BadMap = bad_map,
	BadList = bad_list,
	Default = default,
	[
	% find_nested/2 test with {ok, _}
	?_assertEqual({ok, M1}, find_nested([0], M0)),
	?_assertEqual({ok, M2}, find_nested([0,1], M0)),
	?_assertEqual({ok, M3}, find_nested([0,1,2], M0)),
	?_assertEqual({ok, Value}, find_nested([0,1,2,3], M0)),
	% find_nested/2 test with 'error'
	?_assertEqual(error, find_nested([a], #{})),
	?_assertEqual(error, find_nested([-1], M0)),
	?_assertEqual(error, find_nested([-1, 1], M0)),
	?_assertEqual(error, find_nested([1, -1], M0)),
	?_assertEqual(error, find_nested([0,1,2,-3], M0)),
	% find_nested/2 test with exception
	?_assertError({badmap, BadMap}, find_nested([0], BadMap)),
	?_assertError({badmap, BadMap}, find_nested([0, 1], #{0 => BadMap})),
	?_assertError({badmap, BadMap}, find_nested([0, 1, 2], #{0 => #{1 => BadMap}})),
	?_assertError({badmap, Value}, find_nested([0,1,2,3,4], M0)),
	?_assertError({badmap, Value}, find_nested([0,1,2,3,4,5], M0)),
	?_assertError({badlist, BadList}, find_nested(BadList, #{a => 1})),

	% get_nested/2 test with Value
	?_assertEqual(M1, get_nested([0], M0)),
	?_assertEqual(M2, get_nested([0,1], M0)),
	?_assertEqual(M3, get_nested([0,1,2], M0)),
	?_assertEqual(Value, get_nested([0,1,2,3], M0)),
	% get_nested/2 test with exception
	?_assertError({badkeys, [a]}, get_nested([a], M0)),
	?_assertError({badkeys, [0,-1]}, get_nested([0,-1], M0)),
	?_assertError({badkeys, [0,-1,2]}, get_nested([0,-1,2], M0)),
	?_assertError({badkeys, [0,1,2,-3]}, get_nested([0,1,2,-3], M0)),
	?_assertError({badmap, Value}, get_nested([0,1,2,3,4], M0)),
	?_assertError({badmap, Value}, get_nested([0,1,2,3,4,5], M0)),
	?_assertError({badlist, BadList}, get_nested(BadList, #{a => 1})),

	% get_nested/3 test with Value and Default
	?_assertEqual(M1, get_nested([0], M0, Default)),
	?_assertEqual(M2, get_nested([0,1], M0, Default)),
	?_assertEqual(M3, get_nested([0,1,2], M0, Default)),
	?_assertEqual(Value, get_nested([0,1,2,3], M0, Default)),
	?_assertEqual(Default, get_nested([-1], M0, Default)),
	?_assertEqual(Default, get_nested([0,-1], M0, Default)),
	?_assertEqual(Default, get_nested([0,1,-2], M0, Default)),
	?_assertEqual(Default, get_nested([0,1,2,-3], M0, Default)),
	?_assertEqual(Default, get_nested([0,1,2,3,4], M0, Default)),
	?_assertEqual(Default, get_nested([0,1,2,3,4,5], M0, Default)),
	% get_nested/3 test with exception
	?_assertError({badmap, BadMap}, get_nested([a], BadMap, Default)),
	?_assertError({badlist, BadList}, get_nested(BadList, #{}, Default))
	].

new_nested_test_() ->
	[
	?_assertEqual(#{}, new_nested([])),
	?_assertEqual(#{}, new_nested([], #{})),
	?_assertEqual(#{}, new_nested([], aaa)),
	?_assertEqual(new_nested([]), new_nested([], #{})),
	?_assertEqual(#{1 => #{}}, new_nested([1])),
	?_assertEqual(#{1 => #{2 => #{}}}, new_nested([1,2])),
	?_assertEqual(#{1 => #{2 => #{3 => #{}}}}, new_nested([1,2,3])),
	?_assertEqual(#{1 => value}, new_nested([1], value)),
	?_assertEqual(#{1 => #{2 => value}}, new_nested([1,2], value)),
	?_assertEqual(#{1 => #{2 => #{3 => value}}}, new_nested([1,2,3], value)),
	?_assertEqual(new_nested([]), new_nested([], #{})),
	?_assertEqual(new_nested([1]), new_nested([1], #{})),
	?_assertEqual(new_nested([1,2]), new_nested([1,2], #{})),
	?_assertEqual(new_nested([1,2,3]), new_nested([1,2,3], #{})),
	?_assertError({badlist, bad_list}, new_nested(bad_list)),
	?_assertError({badlist, bad_list}, new_nested(bad_list, value))
	].

-endif. % end of tests
