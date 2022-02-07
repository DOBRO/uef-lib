%% Copyright (c) 2019-2022, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(uef_maps_tests).

-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

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
	?_assertEqual({ok, M1}, uef_maps:find_nested([0], M0)),
	?_assertEqual({ok, M2}, uef_maps:find_nested([0,1], M0)),
	?_assertEqual({ok, M3}, uef_maps:find_nested([0,1,2], M0)),
	?_assertEqual({ok, Value}, uef_maps:find_nested([0,1,2,3], M0)),
	% find_nested/2 test with 'error'
	?_assertEqual(error, uef_maps:find_nested([a], #{})),
	?_assertEqual(error, uef_maps:find_nested([-1], M0)),
	?_assertEqual(error, uef_maps:find_nested([-1, 1], M0)),
	?_assertEqual(error, uef_maps:find_nested([1, -1], M0)),
	?_assertEqual(error, uef_maps:find_nested([0,1,2,-3], M0)),
	% find_nested/2 test with exception
	?_assertError({badmap, BadMap}, uef_maps:find_nested([0], BadMap)),
	?_assertError({badmap, BadMap}, uef_maps:find_nested([0, 1], #{0 => BadMap})),
	?_assertError({badmap, BadMap}, uef_maps:find_nested([0, 1, 2], #{0 => #{1 => BadMap}})),
	?_assertError({badmap, Value}, uef_maps:find_nested([0,1,2,3,4], M0)),
	?_assertError({badmap, Value}, uef_maps:find_nested([0,1,2,3,4,5], M0)),
	?_assertError({badlist, BadList}, uef_maps:find_nested(BadList, #{a => 1})),

	% get_nested/2 test with Value
	?_assertEqual(M1, uef_maps:get_nested([0], M0)),
	?_assertEqual(M2, uef_maps:get_nested([0,1], M0)),
	?_assertEqual(M3, uef_maps:get_nested([0,1,2], M0)),
	?_assertEqual(Value, uef_maps:get_nested([0,1,2,3], M0)),
	% get_nested/2 test with exception
	?_assertError({badkeys, [a]}, uef_maps:get_nested([a], M0)),
	?_assertError({badkeys, [0,-1]}, uef_maps:get_nested([0,-1], M0)),
	?_assertError({badkeys, [0,-1,2]}, uef_maps:get_nested([0,-1,2], M0)),
	?_assertError({badkeys, [0,1,2,-3]}, uef_maps:get_nested([0,1,2,-3], M0)),
	?_assertError({badmap, Value}, uef_maps:get_nested([0,1,2,3,4], M0)),
	?_assertError({badmap, Value}, uef_maps:get_nested([0,1,2,3,4,5], M0)),
	?_assertError({badlist, BadList}, uef_maps:get_nested(BadList, #{a => 1})),

	% get_nested/3 test with Value and Default
	?_assertEqual(M1, uef_maps:get_nested([0], M0, Default)),
	?_assertEqual(M2, uef_maps:get_nested([0,1], M0, Default)),
	?_assertEqual(M3, uef_maps:get_nested([0,1,2], M0, Default)),
	?_assertEqual(Value, uef_maps:get_nested([0,1,2,3], M0, Default)),
	?_assertEqual(Default, uef_maps:get_nested([-1], M0, Default)),
	?_assertEqual(Default, uef_maps:get_nested([0,-1], M0, Default)),
	?_assertEqual(Default, uef_maps:get_nested([0,1,-2], M0, Default)),
	?_assertEqual(Default, uef_maps:get_nested([0,1,2,-3], M0, Default)),
	?_assertEqual(Default, uef_maps:get_nested([0,1,2,3,4], M0, Default)),
	?_assertEqual(Default, uef_maps:get_nested([0,1,2,3,4,5], M0, Default)),
	% get_nested/3 test with exception
	?_assertError({badmap, BadMap}, uef_maps:get_nested([a], BadMap, Default)),
	?_assertError({badlist, BadList}, uef_maps:get_nested(BadList, #{}, Default))
	].

new_nested_test_() ->
	[
	?_assertEqual(#{}, uef_maps:new_nested([])),
	?_assertEqual(#{}, uef_maps:new_nested([], #{})),
	?_assertEqual(#{}, uef_maps:new_nested([], aaa)),
	?_assertEqual(uef_maps:new_nested([]), uef_maps:new_nested([], #{})),
	?_assertEqual(#{1 => #{}}, uef_maps:new_nested([1])),
	?_assertEqual(#{1 => #{2 => #{}}}, uef_maps:new_nested([1,2])),
	?_assertEqual(#{1 => #{2 => #{3 => #{}}}}, uef_maps:new_nested([1,2,3])),
	?_assertEqual(#{1 => value}, uef_maps:new_nested([1], value)),
	?_assertEqual(#{1 => #{2 => value}}, uef_maps:new_nested([1,2], value)),
	?_assertEqual(#{1 => #{2 => #{3 => value}}}, uef_maps:new_nested([1,2,3], value)),
	?_assertEqual(uef_maps:new_nested([]), uef_maps:new_nested([], #{})),
	?_assertEqual(uef_maps:new_nested([1]), uef_maps:new_nested([1], #{})),
	?_assertEqual(uef_maps:new_nested([1,2]), uef_maps:new_nested([1,2], #{})),
	?_assertEqual(uef_maps:new_nested([1,2,3]), uef_maps:new_nested([1,2,3], #{})),
	?_assertError({badlist, bad_list}, uef_maps:new_nested(bad_list)),
	?_assertError({badlist, bad_list}, uef_maps:new_nested(bad_list, value))
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
	?_assertEqual(true, uef_maps:is_key_nested([0], M0)),
	?_assertEqual(true, uef_maps:is_key_nested([0,1], M0)),
	?_assertEqual(true, uef_maps:is_key_nested([0,1,2], M0)),
	?_assertEqual(true, uef_maps:is_key_nested([0,1,2,3], M0)),
	?_assertEqual(false, uef_maps:is_key_nested([], M0)),
	?_assertEqual(false, uef_maps:is_key_nested([-1], M0)),
	?_assertEqual(false, uef_maps:is_key_nested([0,-1], M0)),
	?_assertEqual(false, uef_maps:is_key_nested([0,1,-2], M0)),
	?_assertEqual(false, uef_maps:is_key_nested([0,1,2,-3], M0)),
	?_assertEqual(false, uef_maps:is_key_nested([0,-1,2,3], M0)),
	?_assertEqual(false, uef_maps:is_key_nested([0,1,-2,3], M0)),
	?_assertEqual(false, uef_maps:is_key_nested([0,1,2,3,4], M0)),
	?_assertEqual(false, uef_maps:is_key_nested([0,1,2,3,4,5], M0)),
	?_assertError({badlist, BadList}, uef_maps:is_key_nested(BadList, #{})),
	?_assertError({badmap, BadMap}, uef_maps:is_key_nested([1], BadMap))
	].

put_nested_test_() ->
	Map1 = #{1 => #{2 => #{3 => val3}}},
	NewVal = new_value,
	BadMap = bad_map,
	BadList = bad_list,
	[
	?_assertEqual(Map1, uef_maps:put_nested([], NewVal, Map1)),
	?_assertEqual(#{1 => NewVal}, uef_maps:put_nested([1], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => NewVal}}, uef_maps:put_nested([1,2], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => NewVal}}}, uef_maps:put_nested([1,2,3], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => val3, -3 => NewVal}}}, uef_maps:put_nested([1,2,-3], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => #{4 => NewVal}}}}, uef_maps:put_nested([1,2,3,4], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => val3}}, -1 => NewVal}, uef_maps:put_nested([-1], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => val3}, -2 => NewVal}}, uef_maps:put_nested([1,-2], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => val3, -3 => NewVal}}}, uef_maps:put_nested([1,2,-3], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => #{-4 => NewVal}}}}, uef_maps:put_nested([1,2,3,-4], NewVal, Map1)),
	?_assertError({badlist, BadList}, uef_maps:put_nested(BadList, NewVal, Map1)),
	?_assertError({badmap, BadMap}, uef_maps:put_nested([], NewVal, BadMap))
	].

update_nested_test_() ->
	Map1 = #{1 => #{2 => #{3 => val3}}},
	NewVal = new_value,
	BadMap = bad_map,
	BadList = bad_list,
	[
	?_assertEqual(Map1, uef_maps:update_nested([], NewVal, Map1)),
	?_assertEqual(#{1 => NewVal}, uef_maps:update_nested([1], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => NewVal}}, uef_maps:update_nested([1,2], NewVal, Map1)),
	?_assertEqual(#{1 => #{2 => #{3 => NewVal}}}, uef_maps:update_nested([1,2,3], NewVal, Map1)),
	?_assertError({badkey, -3}, uef_maps:update_nested([1,2,-3], NewVal, Map1)),
	?_assertError({badkey, 4}, uef_maps:update_nested([1,2,3,4], NewVal, Map1)),
	?_assertError({badkey, 4}, uef_maps:update_nested([1,2,3,4,5], NewVal, Map1)),
	?_assertError({badkey, -1}, uef_maps:update_nested([-1], NewVal, Map1)),
	?_assertError({badkey, -2}, uef_maps:update_nested([1,-2], NewVal, Map1)),
	?_assertError({badkey, -3}, uef_maps:update_nested([1,2,-3], NewVal, Map1)),
	?_assertError({badkey, -4}, uef_maps:update_nested([1,2,3,-4], NewVal, Map1)),
	?_assertError({badlist, BadList}, uef_maps:update_nested(BadList, NewVal, Map1)),
	?_assertError({badmap, BadMap}, uef_maps:update_nested([], NewVal, BadMap))
	].

remove_nested_test_() ->
	Map1 = #{1 => #{2 => #{3 => val3}}},
	BadMap = bad_map,
	BadList = bad_list,
	[
	?_assertEqual(Map1, uef_maps:remove_nested([], Map1)),
	?_assertEqual(#{}, uef_maps:remove_nested([1], Map1)),
	?_assertEqual(#{1 => #{}}, uef_maps:remove_nested([1,2], Map1)),
	?_assertEqual(#{1 => #{2 => #{}}}, uef_maps:remove_nested([1,2,3], Map1)),
	?_assertEqual(Map1, uef_maps:remove_nested([-1], Map1)),
	?_assertEqual(Map1, uef_maps:remove_nested([1,-2], Map1)),
	?_assertEqual(Map1, uef_maps:remove_nested([1,2,-3], Map1)),
	?_assertEqual(Map1, uef_maps:remove_nested([1,2,3,4], Map1)),
	?_assertEqual(Map1, uef_maps:remove_nested([1,2,3,4,5], Map1)),
	?_assertError({badlist, BadList}, uef_maps:remove_nested(BadList, Map1)),
	?_assertError({badmap, BadMap}, uef_maps:remove_nested([], BadMap))
	].

take_nested_test_() ->
	Map1 = #{1 => #{2 => #{3 => val3}}},
	BadMap = bad_map,
	BadList = bad_list,
	[
	?_assertEqual(error, uef_maps:take_nested([], Map1)),
	?_assertEqual({ #{2 => #{3 => val3}}, #{} }, uef_maps:take_nested([1], Map1)),
	?_assertEqual({ #{3 => val3}, #{1 => #{}} }, uef_maps:take_nested([1,2], Map1)),
	?_assertEqual({ val3, #{1 => #{2 => #{}}} }, uef_maps:take_nested([1,2,3], Map1)),
	?_assertEqual(error, uef_maps:take_nested([-1], Map1)),
	?_assertEqual(error, uef_maps:take_nested([1,-2], Map1)),
	?_assertEqual(error, uef_maps:take_nested([1,2,-3], Map1)),
	?_assertEqual(error, uef_maps:take_nested([1,2,3,4], Map1)),
	?_assertEqual(error, uef_maps:take_nested([1,2,3,4,5], Map1)),
	?_assertError({badlist, BadList}, uef_maps:take_nested(BadList, Map1)),
	?_assertError({badmap, BadMap}, uef_maps:take_nested([], BadMap))
	].

delete_nested_test_() ->
	Map1 = #{1 => #{2 => #{3 => val3}}},
	BadMap = bad_map,
	BadList = bad_list,
	[
	?_assertEqual({error, empty_keys}, uef_maps:delete_nested([], Map1)),
	?_assertEqual({ok, #{}}, uef_maps:delete_nested([1], Map1)),
	?_assertEqual({ok, #{1 => #{}}}, uef_maps:delete_nested([1,2], Map1)),
	?_assertEqual({ ok, #{1 => #{2 => #{}}} }, uef_maps:delete_nested([1,2,3], Map1)),
	?_assertEqual({error, {badkey, -1}}, uef_maps:delete_nested([-1], Map1)),
	?_assertEqual({error, {badkey, -2}}, uef_maps:delete_nested([1,-2], Map1)),
	?_assertEqual({error, {badkey, -3}}, uef_maps:delete_nested([1,2,-3], Map1)),
	?_assertEqual({error, {badkey, 4}}, uef_maps:delete_nested([1,2,3,4], Map1)),
	?_assertEqual({error, {badkey, 4}}, uef_maps:delete_nested([1,2,3,4,5], Map1)),
	?_assertError({badlist, BadList}, uef_maps:delete_nested(BadList, Map1)),
	?_assertError({badmap, BadMap}, uef_maps:delete_nested([], BadMap))
	].
