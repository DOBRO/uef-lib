%% Copyright (c) 2019-2024, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(uef_num).

-export([round_price/1, round_number/2]).
-export([popcount/1, msb_pos/1, lsb_pos/1, ctz/1]).


%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% round_price/1
-spec round_price(Number :: number()) -> float().
%% @doc
%% Rounds the number to the precision of 2. The same as uef_num:round_number(Number, 2).
%% @end
round_price(Price) -> round_number(Price, 2).

%% round_number/2
-spec round_number(Number :: number(), Precision :: integer()) -> float().
%% @doc
%% Rounds the number to the specified precision.
%% @end
round_number(Number, Precision) ->
    P = math:pow(10, Precision),
    erlang:round(Number * P) / P.


%% popcount/1
-spec popcount(Integer:: non_neg_integer()) -> OneBits :: non_neg_integer().
%% @doc
%% Returns the number of 1's (ones or one-bits) in the binary representation of a non-negative integer.
%% Also known as population count, pop count, popcount, sideways sum, bit summation,
%% or Hamming weight.
%% The call fails with a {badarg,Integer} exception if Integer is not a non-negative integer.
%% @end
popcount(N) when is_integer(N) andalso (N > -1) ->
    popcount(N, 0);
popcount(N) ->
    erlang:error({badarg, N}, [N]).

%% lsb_pos/1
-spec lsb_pos(Integer:: pos_integer()) -> Pos :: pos_integer().
%% @doc
%% Returns the position of the least significant bit in the binary representation of a positive integer.
%% The call fails with a {badarg,Integer} exception if Integer is not a positive integer.
%% @end
lsb_pos(N) when is_integer(N) andalso (N > 0) ->
    lsb_pos(N, 1);
lsb_pos(N) ->
    erlang:error({badarg, N}, [N]).

%% msb_pos/1
-spec msb_pos(Integer:: pos_integer()) -> Pos :: pos_integer().
%% @doc
%% Returns the position of the most significant bit in the binary representation of a positive integer.
%% The call fails with a {badarg,Integer} exception if Integer is not a positive integer.
%% @end
msb_pos(N) when is_integer(N) andalso (N > 0) ->
    msb_pos(N, 0);
msb_pos(N) ->
    erlang:error({badarg, N}, [N]).

%% ctz/1
-spec ctz(Integer:: pos_integer()) -> TrailingZeros :: non_neg_integer().
%% @doc
%% Counts trailing zeros in the binary representation of a positive integer.
%% Returns the number of zero bits following the least significant one bit.
%% The call fails with a {badarg,Integer} exception if Integer is not a positive integer.
%% @end
ctz(N) ->
    lsb_pos(N) - 1.


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% popcount/2
-spec popcount(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
popcount(0, Cnt) -> Cnt;
popcount(N, Cnt) -> popcount(N band (N - 1), Cnt + 1).


%% lsb_pos/2
-spec lsb_pos(pos_integer(), pos_integer()) -> pos_integer().
lsb_pos(N, Cnt) when ((N band 1) =:= 1) -> Cnt;
lsb_pos(N, Cnt) -> lsb_pos(N bsr 1, Cnt + 1).

%% msb_pos/2
-spec msb_pos(non_neg_integer(), non_neg_integer()) -> pos_integer().
msb_pos(0, Cnt) -> Cnt;
msb_pos(N, Cnt) -> msb_pos(N bsr 1, Cnt + 1).
