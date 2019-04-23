-module(uef_lists_tests).
-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Generator
%%%------------------------------------------------------------------------------

uef_lists_test_() ->
	[
		test_split_list_into_chunks(),
		test_lists_to_list_of_tuples_2(),
		test_lists_to_list_of_tuples_3()
	].


%%%------------------------------------------------------------------------------
%%%   Test functions
%%%------------------------------------------------------------------------------

test_split_list_into_chunks() ->
	Fun = fun uef_lists:split_list_into_chunks/2,
	[
		?_assertEqual([[1],[2],[3],[4],[5],[6],[7],[8]], Fun([1,2,3,4,5,6,7,8], 1)),
		?_assertEqual([[1,2],[3,4],[5,6],[7,8]], Fun([1,2,3,4,5,6,7,8], 2)),
		?_assertEqual([[1,2,3],[4,5,6],[7,8]], Fun([1,2,3,4,5,6,7,8], 3)),
		?_assertEqual([[1,2,3,4],[5,6,7,8]], Fun([1,2,3,4,5,6,7,8], 4)),
		?_assertEqual([[1,2,3,4,5,6,7,8]], Fun([1,2,3,4,5,6,7,8], 8)),
		?_assertEqual([[1,2,3,4,5,6,7,8]], Fun([1,2,3,4,5,6,7,8], 9)),
		?_assertEqual([[1,2,3,4,5,6,7,8]], Fun([1,2,3,4,5,6,7,8], 99))
	].

test_lists_to_list_of_tuples_2() ->
	Fun = fun uef_lists:lists_to_list_of_tuples/2,
	[
		?_assertEqual([{a,1},{a,2},{b,1},{b,2},{c,1},{c,2}], Fun([a,b,c], [1,2])),
		?_assertEqual([{a,1},{a,2},{a,3},{b,1},{b,2},{b,3},{c,1},{c,2},{c,3}], Fun([a,b,c], [1,2,3]))
	].

test_lists_to_list_of_tuples_3() ->
	Fun = fun uef_lists:lists_to_list_of_tuples/3,
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
		?_assertEqual(Tuples1, Fun([a1,b1], [a2,b2,c2], [a3,b3]))
	].
