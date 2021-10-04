%% Copyright (c) 2019-2021, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(uef_lists_tests).

-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

split_list_into_chunks_test_() ->
	[
	?_assertEqual([[1],[2],[3],[4],[5],[6],[7],[8]], uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 1)),
	?_assertEqual([[1,2],[3,4],[5,6],[7,8]], uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 2)),
	?_assertEqual([[1,2,3],[4,5,6],[7,8]], uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 3)),
	?_assertEqual([[1,2,3,4],[5,6,7,8]], uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 4)),
	?_assertEqual([[1,2,3,4,5,6,7,8]], uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 8)),
	?_assertEqual([[1,2,3,4,5,6,7,8]], uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 9)),
	?_assertEqual([[1,2,3,4,5,6,7,8]], uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 99))
	].

lists_to_list_of_tuples_2_test_() ->
	[
	?_assertEqual([{a,1},{a,2},{b,1},{b,2},{c,1},{c,2}], uef_lists:lists_to_list_of_tuples([a,b,c], [1,2])),
	?_assertEqual([{a,1},{a,2},{a,3},{b,1},{b,2},{b,3},{c,1},{c,2},{c,3}], uef_lists:lists_to_list_of_tuples([a,b,c], [1,2,3]))
	].

lists_to_list_of_tuples_3_test_() ->
	Tuples1 = [
		{a1,a2,a3},
		{a1,a2,b3},
		{a1,b2,a3},
		{a1,b2,b3},
		{a1,c2,a3},
		{a1,c2,b3},
		{b1,a2,a3},
		{b1,a2,b3},
		{b1,b2,a3},
		{b1,b2,b3},
		{b1,c2,a3},
		{b1,c2,b3}
	],
	[
	?_assertEqual(Tuples1, uef_lists:lists_to_list_of_tuples([a1,b1], [a2,b2,c2], [a3,b3]))
	].
