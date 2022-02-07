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

-module(uef_lists).

-export([split_list_into_chunks/2]).
-export([lists_to_list_of_tuples/2, lists_to_list_of_tuples/3]).
-export([search/2]).

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% split_list_into_chunks/2
-spec split_list_into_chunks(List :: list(), MaxLen :: pos_integer()) -> List2 :: list().
%% @doc
%% Splits List into list of lists [List1, List2, ..., ListN]
%% where List1, List2, ..., ListN are lists with maximum MaxLen elements.
%% @end
split_list_into_chunks([],_) -> [];
split_list_into_chunks(List,Len) when Len > length(List) ->
    [List];
split_list_into_chunks(List,Len) ->
    {Head,Tail} = lists:split(Len,List),
    [Head | split_list_into_chunks(Tail,Len)].


%% lists_to_list_of_tuples/2
-spec lists_to_list_of_tuples(List1 :: list(), List2 :: list()) -> List3 :: [tuple()].
%% @doc
%% Transforms two lists into one list of two-tuples,
%% where the first element of each tuple is taken from the first list
%% and the second element is taken from the second list one by one.
%% @end
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
-spec lists_to_list_of_tuples(List1 :: list(), List2 :: list(), List3 :: list()) -> List4 :: [tuple()].
%% @doc
%% Transforms three lists into one list of three-tuples,
%% where the first element of each tuple is taken from the first list,
%% the second element is taken from the second list one by one,
%% and the third element is taken from the third list one by one.
%% @end
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

%% search/2
-spec search(Pred, List) -> {value, Value} | false when
      Pred :: fun((T) -> boolean()),
      List :: [T],
      Value :: T.
%% @doc
%% If there is a Value in List such that Pred(Value) returns true, returns {value, Value} for the first such Value, otherwise returns false.
%% Since OTP 21.0 use BIF lists:search/2 instead.
%% @end
search(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
        true -> {value, Hd};
        false -> search(Pred, Tail)
    end;
search(Pred, []) when is_function(Pred, 1) ->
    false.
