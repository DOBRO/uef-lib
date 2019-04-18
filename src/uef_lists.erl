-module(uef_lists).

-export([split_list_into_chunks/2]).
-export([lists_to_list_of_tuples/2, lists_to_list_of_tuples/3]).


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
