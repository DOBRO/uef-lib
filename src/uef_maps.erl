%% Copyright (c) 2019-2020, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(uef_maps).

-export([delete_nested/2]).
-export([find_nested/2]).
-export([get_nested/2, get_nested/3]).
-export([new_nested/1, new_nested/2]).
-export([is_key_nested/2]).
-export([put_nested/3]).
-export([update_nested/3]).
-export([remove_nested/2]).
-export([take_nested/2]).

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
%% @doc
%% Returns tuple {ok, Value}, where Value is the value associated with the last element of list Keys, or error if no value is found.
%% The call fails with a {badmap,Map} exception if Map is not a map, or with a {badlist,Keys} exception if Keys is not a list.
%% @end
find_nested(Keys, Map) ->
	find_nested_unsafe(Keys, Map).


%% get_nested/2
-spec get_nested(Keys :: mapkeys(), Map :: map()) -> Value :: term().
%% @doc
%% Returns value Value associated with the last element of list Keys.
%% The call fails with a {badmap,Map} exception if Map is not a map,
%% or with a {badkeys,Keys} exception if no value is found,
%% or with a {badlist,Keys} exception if Keys is not a list.
%% @end
get_nested(Keys, Map) ->
	FindResult = find_nested_unsafe(Keys, Map), % may fail here with a {badmap,Map} exception
	case FindResult of
		{ok, Value} -> Value;
		error -> erlang:error({badkeys,Keys},[Keys,Map])
	end.

%% get_nested/3
-spec get_nested(Keys :: mapkeys(), Map :: map(), Default :: term()) -> Value :: term().
%% @doc
%% Returns value Value associated with the last element of list Keys.
%% If no value is found, Default is returned.
%% The call fails with a {badmap,Map} exception if Map is not a map,
%% or with a {badlist,Keys} exception if Keys is not a list.
%% It does not fail if any internal value associated with any element of list Keys is not a map.
%% @end
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
%% @doc
%% Same as uef_maps:new_nested(Keys, #{}). See docs of uef_maps:new_nested/2.
%% @end
new_nested(Keys) ->
	new_nested(Keys, #{}).

%% new_nested/2
-spec new_nested(Keys :: mapkeys(), Value :: term()) -> Map :: map().
%% @doc
%% Returns new nested map Map with the deepest map #{LastKey => Value}, where LastKey is the last element of list Keys.
%% The call fails with a {badlist,Keys} exception if Keys is not a list.
%% @end
new_nested([], _) ->
	#{};
new_nested([Key], Value) ->
	#{Key => Value};
new_nested(Keys, Value) when is_list(Keys) ->
	[LastKey | Rest] = lists:reverse(Keys),
	new_nested_1(Rest, #{LastKey => Value});
new_nested(Keys, Value) ->
	erlang:error({badlist, Keys}, [Keys, Value]).


%% is_key_nested/2
-spec is_key_nested(Keys :: mapkeys(), map()) -> boolean().
%% @doc
%% Returns true if map Map contains submaps as values associated with their own key corresponding to the element of list Keys,
%% and returns false otherwise.
%% The call fails with a {badmap,Map} exception if Map is not a map,
%% or with a {badlist,Keys} exception if Keys is not a list.
%% @end
is_key_nested(Keys, Map) when is_list(Keys), is_map(Map) ->
	is_key_nested(Keys, Map, false);
is_key_nested(Keys, Map) ->
	Args = [Keys, Map],
	case is_list(Keys) of
		true  -> erlang:error({badmap, Map}, Args);
		false -> erlang:error({badlist, Keys}, Args)
	end.


%% put_nested/3
-spec put_nested(Keys :: mapkeys(), Value :: term(), Map1 :: map()) -> Map2 :: map().
%% @doc
%% The function associates KeyN with value Value and updates the entire structure of map Map1 returning new map Map2.
%% If some keys from list Keys are not in the structure of map Map1, they will be inserted into the structure of map Map2 in the same order.
%% The call fails with a {badmap,Map1} exception if Map1 is not a map, or with a {badlist,Keys} exception if Keys is not a list.
%% @end
put_nested([], _, Map) when is_map(Map) ->
	Map;
put_nested([Key], Value, Map) when is_map(Map) ->
	Map#{Key => Value};
put_nested(Keys, Value, Map) when is_list(Keys), is_map(Map) ->
	Tuples = nested_to_tuples_for_put(Keys, Map, []),
	lists:foldl(fun({K, M}, Acc) -> M#{K => Acc} end, Value, Tuples);
put_nested(Keys, Value, Map) ->
	Args = [Keys, Value, Map],
	case is_list(Keys) of
		true  -> erlang:error({badmap, Map}, Args);
		false -> erlang:error({badlist, Keys}, Args)
	end.


%% update_nested/3
-spec update_nested(mapkeys(), Value :: term(), map()) -> map().
%% @doc
%% Works similar to uef_maps:put_nested/3 with the difference that it fails with a {badkey,SomeKey} exception
%% if SomeKey does not exist in the structure of map Map1, where SomeKey is one of the elements of list Keys.
%% The call also fails with a {badmap,Map1} exception if Map1 is not a map, or with a {badlist,Keys} exception if Keys is not a list.
%% @end
update_nested([], _, Map) when is_map(Map) ->
	Map;
update_nested([Key], Value, Map) when is_map(Map) ->
	maps:update(Key, Value, Map);
update_nested(Keys, Value, Map) when is_list(Keys), is_map(Map) ->
	case nested_to_tuples_for_update(Keys, Map, []) of
		{ok, Tuples} ->
			lists:foldl(fun({K, M}, Acc) -> maps:update(K, Acc, M) end, Value, Tuples);
		{error, Reason} ->
			erlang:error(Reason, [Keys, Value, Map])
	end;
update_nested(Keys, Value, Map) ->
	Args = [Keys, Value, Map],
	case is_list(Keys) of
		true  -> erlang:error({badmap, Map}, Args);
		false -> erlang:error({badlist, Keys}, Args)
	end.


%% remove_nested/2
-spec remove_nested(Keys :: mapkeys(), Map1 :: map()) -> Map2 :: map().
%% @doc
%% The function removes key KeyN, if it exists, and its associated value from the corresponding internal map
%% and updates the entire structure of map Map1 returning new map Map2.
%% If some keys from list Keys are not in the structure of map Map1 the function returns a map without changes.
%% The call fails with a {badmap,Map1} exception if Map1 is not a map, or with a {badlist,Keys} exception if Keys is not a list.
%% @end
remove_nested([], Map) when is_map(Map) ->
	Map;
remove_nested([Key], Map) when is_map(Map) ->
	maps:remove(Key, Map);
remove_nested(Keys, Map) when is_list(Keys), is_map(Map) ->
	case nested_to_tuples_for_update(Keys, Map, []) of
		{ok, Tuples} ->
			[{LastKey, LastMap} | Rest] = Tuples,
			LastMap2 = maps:remove(LastKey, LastMap),
			lists:foldl(fun({K, M}, Acc) -> M#{K => Acc} end, LastMap2, Rest);
		{error, _} ->
			Map
	end;
remove_nested(Keys, Map) ->
	Args = [Keys, Map],
	case is_list(Keys) of
		true  -> erlang:error({badmap, Map}, Args);
		false -> erlang:error({badlist, Keys}, Args)
	end.


%% take_nested/2
-spec take_nested(Keys :: mapkeys(), Map1 :: map()) -> {Value :: term(), Map2 :: map()} | error.
%% @doc
%% The function removes key KeyN, if it exists, and its associated value Value from the corresponding internal map
%% and updates the entire structure of map Map1 returning tuple {Value, Map2}.
%% If some keys from list Keys are not in the structure of map Map1 the function returns error.
%% The call fails with a {badmap,Map1} exception if Map1 is not a map, or with a {badlist,Keys} exception if Keys is not a list.
%% @end
take_nested([], Map) when is_map(Map) ->
	error;
take_nested([Key], Map) when is_map(Map) ->
	maps:take(Key, Map);
take_nested(Keys, Map) when is_list(Keys), is_map(Map) ->
	case nested_to_tuples_for_update(Keys, Map, []) of
		{ok, Tuples} ->
			[{LastKey, LastMap} | Rest] = Tuples,
			{Value, LastMap2} = maps:take(LastKey, LastMap),
			Map2 = lists:foldl(fun({K, M}, Acc) -> M#{K => Acc} end, LastMap2, Rest),
			{Value, Map2};
		{error, _} ->
			error
	end;
take_nested(Keys, Map) ->
	Args = [Keys, Map],
	case is_list(Keys) of
		true  -> erlang:error({badmap, Map}, Args);
		false -> erlang:error({badlist, Keys}, Args)
	end.


%% delete_nested/2
-spec delete_nested(Keys :: mapkeys(), Map1 :: map()) -> {ok, Map2 :: map()} | {error, {badkey, mapkey()}} | {error, empty_keys}.
%% @doc
%% The function removes key KeyN, if it exists, and its associated value from the corresponding internal map
%% and updates the entire structure of map Map1 getting new map Map2. KeyN is the last element of list Keys.
%% @end
delete_nested([], Map) when is_map(Map) ->
	{error, empty_keys};
delete_nested([Key], Map) when is_map(Map) ->
	case maps:is_key(Key, Map) of
		true  -> {ok, maps:remove(Key, Map)};
		false -> {error, {badkey, Key}}
	end;
delete_nested(Keys, Map) when is_list(Keys), is_map(Map) ->
	case nested_to_tuples_for_update(Keys, Map, []) of
		{ok, Tuples} ->
			[{LastKey, LastMap} | Rest] = Tuples,
			LastMap2 = maps:remove(LastKey, LastMap),
			Map2 = lists:foldl(fun({K, M}, Acc) -> M#{K => Acc} end, LastMap2, Rest),
			{ok, Map2};
		{error, _} = Error ->
			Error
	end;
delete_nested(Keys, Map) ->
	Args = [Keys, Map],
	case is_list(Keys) of
		true  -> erlang:error({badmap, Map}, Args);
		false -> erlang:error({badlist, Keys}, Args)
	end.

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

%% is_key_nested/3
-spec is_key_nested(mapkeys(), map(), boolean()) -> boolean().
is_key_nested([], _, Bool) ->
	Bool;
is_key_nested([Key|Tail], Map, _) ->
	case Map of
		#{Key := Value} -> is_key_nested(Tail, Value, true);
		_ -> false
	end.

%% nested_to_tuples_for_put/3
-spec nested_to_tuples_for_put(mapkeys(), term(), Tuples) -> Tuples when Tuples :: [{mapkey(), map()}].
nested_to_tuples_for_put([], _Map, Tuples) ->
	Tuples;
nested_to_tuples_for_put([Key|Tail], Map, Tuples) ->
	case Map of
		#{Key := M} ->
			nested_to_tuples_for_put(Tail, M, [{Key, Map}|Tuples]);
		_ when is_map(Map) ->
			nested_to_tuples_for_put(Tail, #{}, [{Key, Map}|Tuples]);
		_ ->
			nested_to_tuples_for_put(Tail, #{}, [{Key, #{}}|Tuples])
	end.

%% nested_to_tuples_for_update/3
-spec nested_to_tuples_for_update(mapkeys(), term(), Tuples) -> {ok, Tuples} | {error, {badkey, mapkey()}}
	when Tuples :: [{mapkey(), map()}].
nested_to_tuples_for_update([], _Map, Tuples) ->
	{ok, Tuples};
nested_to_tuples_for_update([Key|Tail], Map, Tuples) ->
	case Map of
		#{Key := M} ->
			nested_to_tuples_for_update(Tail, M, [{Key, Map}|Tuples]);
		_ ->
			{error, {badkey, Key}}
	end.

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

is_key_nested_test_() ->
	Value = value,
	M3 = #{3 => Value},
	M2 = #{2 => M3},
	M1 = #{1 => M2},
	M0 = #{0 => M1},
	BadMap = bad_map,
	BadList = bad_list,
	[
	?_assertEqual(true, is_key_nested([0], M0)),
	?_assertEqual(true, is_key_nested([0,1], M0)),
	?_assertEqual(true, is_key_nested([0,1,2], M0)),
	?_assertEqual(true, is_key_nested([0,1,2,3], M0)),
	?_assertEqual(false, is_key_nested([], M0)),
	?_assertEqual(false, is_key_nested([-1], M0)),
	?_assertEqual(false, is_key_nested([0,-1], M0)),
	?_assertEqual(false, is_key_nested([0,1,-2], M0)),
	?_assertEqual(false, is_key_nested([0,1,2,-3], M0)),
	?_assertEqual(false, is_key_nested([0,-1,2,3], M0)),
	?_assertEqual(false, is_key_nested([0,1,-2,3], M0)),
	?_assertEqual(false, is_key_nested([0,1,2,3,4], M0)),
	?_assertEqual(false, is_key_nested([0,1,2,3,4,5], M0)),
	?_assertError({badlist, BadList}, is_key_nested(BadList, #{})),
	?_assertError({badmap, BadMap}, is_key_nested([1], BadMap))
	].

put_nested_test_() ->
	Map1 = #{1 => #{2 => #{3 => val3}}},
	NewVal = new_value,
	BadMap = bad_map,
	BadList = bad_list,
	[
	?_assertEqual(Map1, put_nested([], NewVal, Map1)),
	?_assertEqual(#{1 => NewVal}, put_nested([1], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => NewVal}}, put_nested([1,2], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => NewVal}}}, put_nested([1,2,3], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => val3, -3 => NewVal}}}, put_nested([1,2,-3], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => #{4 => NewVal}}}}, put_nested([1,2,3,4], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => val3}}, -1 => NewVal}, put_nested([-1], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => val3}, -2 => NewVal}}, put_nested([1,-2], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => val3, -3 => NewVal}}}, put_nested([1,2,-3], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => #{-4 => NewVal}}}}, put_nested([1,2,3,-4], NewVal, Map1)),
	?_assertError({badlist, BadList}, put_nested(BadList, NewVal, Map1)),
	?_assertError({badmap, BadMap}, put_nested([], NewVal, BadMap))
	].

update_nested_test_() ->
	Map1 = #{1 => #{2 => #{3 => val3}}},
	NewVal = new_value,
	BadMap = bad_map,
	BadList = bad_list,
	[
	?_assertEqual(Map1, update_nested([], NewVal, Map1)),
	?_assertEqual(#{1 => NewVal}, update_nested([1], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => NewVal}}, update_nested([1,2], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => NewVal}}}, update_nested([1,2,3], NewVal, Map1)),
	?_assertError({badkey, -3}, update_nested([1,2,-3], NewVal, Map1)),
	?_assertError({badkey, 4}, update_nested([1,2,3,4], NewVal, Map1)),
	?_assertError({badkey, 4}, update_nested([1,2,3,4,5], NewVal, Map1)),
	?_assertError({badkey, -1}, update_nested([-1], NewVal, Map1)),
	?_assertError({badkey, -2}, update_nested([1,-2], NewVal, Map1)),
	?_assertError({badkey, -3}, update_nested([1,2,-3], NewVal, Map1)),
	?_assertError({badkey, -4}, update_nested([1,2,3,-4], NewVal, Map1)),
	?_assertError({badlist, BadList}, update_nested(BadList, NewVal, Map1)),
	?_assertError({badmap, BadMap}, update_nested([], NewVal, BadMap))
	].

remove_nested_test_() ->
	Map1 = #{1 => #{2 => #{3 => val3}}},
	BadMap = bad_map,
	BadList = bad_list,
	[
	?_assertEqual(Map1, remove_nested([], Map1)),
	?_assertEqual(#{}, remove_nested([1], Map1)),
	?_assertEqual(#{1 => #{}}, remove_nested([1,2], Map1)),
	?_assertEqual(#{1 => #{2 => #{}}}, remove_nested([1,2,3], Map1)),
	?_assertEqual(Map1, remove_nested([-1], Map1)),
	?_assertEqual(Map1, remove_nested([1,-2], Map1)),
	?_assertEqual(Map1, remove_nested([1,2,-3], Map1)),
	?_assertEqual(Map1, remove_nested([1,2,3,4], Map1)),
	?_assertEqual(Map1, remove_nested([1,2,3,4,5], Map1)),
	?_assertError({badlist, BadList}, remove_nested(BadList, Map1)),
	?_assertError({badmap, BadMap}, remove_nested([], BadMap))
	].

take_nested_test_() ->
	Map1 = #{1 => #{2 => #{3 => val3}}},
	BadMap = bad_map,
	BadList = bad_list,
	[
	?_assertEqual(error, take_nested([], Map1)),
	?_assertEqual({ #{2 => #{3 => val3}}, #{} }, take_nested([1], Map1)),
	?_assertEqual({ #{3 => val3}, #{1 => #{}} }, take_nested([1,2], Map1)),
	?_assertEqual({ val3, #{1 => #{2 => #{}}} }, take_nested([1,2,3], Map1)),
	?_assertEqual(error, take_nested([-1], Map1)),
	?_assertEqual(error, take_nested([1,-2], Map1)),
	?_assertEqual(error, take_nested([1,2,-3], Map1)),
	?_assertEqual(error, take_nested([1,2,3,4], Map1)),
	?_assertEqual(error, take_nested([1,2,3,4,5], Map1)),
	?_assertError({badlist, BadList}, take_nested(BadList, Map1)),
	?_assertError({badmap, BadMap}, take_nested([], BadMap))
	].

delete_nested_test_() ->
	Map1 = #{1 => #{2 => #{3 => val3}}},
	BadMap = bad_map,
	BadList = bad_list,
	[
	?_assertEqual({error, empty_keys}, delete_nested([], Map1)),
	?_assertEqual({ok, #{}}, delete_nested([1], Map1)),
	?_assertEqual({ok, #{1 => #{}}}, delete_nested([1,2], Map1)),
	?_assertEqual({ ok, #{1 => #{2 => #{}}} }, delete_nested([1,2,3], Map1)),
	?_assertEqual({error, {badkey, -1}}, delete_nested([-1], Map1)),
	?_assertEqual({error, {badkey, -2}}, delete_nested([1,-2], Map1)),
	?_assertEqual({error, {badkey, -3}}, delete_nested([1,2,-3], Map1)),
	?_assertEqual({error, {badkey, 4}}, delete_nested([1,2,3,4], Map1)),
	?_assertEqual({error, {badkey, 4}}, delete_nested([1,2,3,4,5], Map1)),
	?_assertError({badlist, BadList}, delete_nested(BadList, Map1)),
	?_assertError({badmap, BadMap}, delete_nested([], BadMap))
	].

-endif. % end of tests
