-module(uef_lists).

-export([split_list_into_chunks/2]).
-export([lists_to_list_of_tuples/2, lists_to_list_of_tuples/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% split_list_into_chunks/2
%% split_list_into_chunks([1,2,3,4,5,6,7,8], 3) -> [[1,2,3], [4,5,6], [7,8]]
-spec split_list_into_chunks(list(), pos_integer()) -> list().
split_list_into_chunks([],_) -> [];
split_list_into_chunks(List,Len) when Len > length(List) ->
	[List];
split_list_into_chunks(List,Len) ->
	{Head,Tail} = lists:split(Len,List),
	[Head | split_list_into_chunks(Tail,Len)].


%% lists_to_list_of_tuples/2
%% lists_to_list_of_tuples([a,b,c], [1,2]) -> [{a,1},{a,2},{b,1},{b,2},{c,1},{c,2}]
-spec lists_to_list_of_tuples(list(), list()) -> [tuple()].
lists_to_list_of_tuples(List1, List2) ->
	List = lists:foldl(
		fun(Elem1, Acc1) ->
			lists:foldl(
				fun(Elem2, Acc2) ->
					[{Elem1, Elem2} | Acc2]
				end,
				Acc1, List2
			)
		end,
		[], List1
	),
	lists:reverse(List).

%% lists_to_list_of_tuples/3
%% lists_to_list_of_tuples([a1,b1,c1], [a2,b2,c2], [a3,b3,c3]) ->
%% [{a1,a2,a3},{a1,a2,b3}, {a1,a2,c3},{a1,b2,a3},{a1,b2,b3}, ...]
-spec lists_to_list_of_tuples(list(), list(), list()) -> [tuple()].
lists_to_list_of_tuples(List1, List2, List3) ->
	List = lists:foldl(
		fun(Elem1, Acc1) ->
			lists:foldl(
				fun(Elem2, Acc2) ->
					lists:foldl(
						fun(Elem3, Acc3) ->
							[{Elem1, Elem2, Elem3} | Acc3]
						end,
						Acc2, List3
					)
				end,
				Acc1, List2
			)
		end,
		[], List1
	),
	lists:reverse(List).


%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

-ifdef(TEST).

split_list_into_chunks_test_() ->
	[
	?_assertEqual([[1],[2],[3],[4],[5],[6],[7],[8]], split_list_into_chunks([1,2,3,4,5,6,7,8], 1)),
	?_assertEqual([[1,2],[3,4],[5,6],[7,8]], split_list_into_chunks([1,2,3,4,5,6,7,8], 2)),
	?_assertEqual([[1,2,3],[4,5,6],[7,8]], split_list_into_chunks([1,2,3,4,5,6,7,8], 3)),
	?_assertEqual([[1,2,3,4],[5,6,7,8]], split_list_into_chunks([1,2,3,4,5,6,7,8], 4)),
	?_assertEqual([[1,2,3,4,5,6,7,8]], split_list_into_chunks([1,2,3,4,5,6,7,8], 8)),
	?_assertEqual([[1,2,3,4,5,6,7,8]], split_list_into_chunks([1,2,3,4,5,6,7,8], 9)),
	?_assertEqual([[1,2,3,4,5,6,7,8]], split_list_into_chunks([1,2,3,4,5,6,7,8], 99))
	].

lists_to_list_of_tuples_2_test_() ->
	[
	?_assertEqual([{a,1},{a,2},{b,1},{b,2},{c,1},{c,2}], lists_to_list_of_tuples([a,b,c], [1,2])),
	?_assertEqual([{a,1},{a,2},{a,3},{b,1},{b,2},{b,3},{c,1},{c,2},{c,3}], lists_to_list_of_tuples([a,b,c], [1,2,3]))
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
	?_assertEqual(Tuples1, lists_to_list_of_tuples([a1,b1], [a2,b2,c2], [a3,b3]))
	].

-endif. % end of tests
