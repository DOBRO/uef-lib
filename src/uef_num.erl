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

-module(uef_num).

-export([round_price/1, round_number/2]).
-export([popcount/1, msb_pos/1, lsb_pos/1, ctz/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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


%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

-ifdef(TEST).

round_number_test_() ->
	[
	?_assertEqual(1.0, round_price(1)),
	?_assertEqual(1.01, round_price(1.01)),
	?_assertEqual(1.01, round_price(1.015)),
	?_assertEqual(1.02, round_price(1.025)),
	?_assertEqual(1.02, round_price(1.0155)),
	?_assertEqual(1.015, round_number(1.015, 3)),
	?_assertEqual(2.0, round_number(1.9999, 1)),
	?_assertEqual(2.0, round_number(1.9999, 2)),
	?_assertEqual(1.9999, round_number(1.9999, 4)),
	?_assertEqual(-1.9999, round_number(-1.9999, 4)),
	?_assertEqual(-2.0, round_number(-1.9999, 3)),
	?_assertEqual(10000.0, round_number(9999.999999, 5))
	].


popcount_test_() ->
	[
		?_assertEqual(0, popcount(0)),
		?_assertEqual(3, popcount(7)),
		?_assertEqual(1, popcount(8)),
		?_assertEqual(1, popcount(8)),
		?_assertEqual(1, popcount(2#0000000000000000000000000000000000000000000000000000000000000001)),
		?_assertEqual(1, popcount(2#1000000000000000000000000000000000000000000000000000000000000000)),
		?_assertEqual(8, popcount(2#0000000100000001000000010000000100000001000000010000000100000001)),
		?_assertEqual(8, popcount(2#0000000000000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(16, popcount(2#1111111100000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(64, popcount(2#1111111111111111111111111111111111111111111111111111111111111111)),
		?_assertError({badarg, -1}, popcount(-1)),
		?_assertError({badarg, 1.0}, popcount(1.0)),
		?_assertError({badarg, 0.0}, popcount(0.0))
	].


msb_pos_test_() ->
	[
		?_assertEqual(1, msb_pos(2#1)),
		?_assertEqual(1, msb_pos(2#01)),
		?_assertEqual(2, msb_pos(2#10)),
		?_assertEqual(3, msb_pos(2#100)),
		?_assertEqual(3, msb_pos(2#0100)),
		?_assertEqual(3, msb_pos(2#111)),
		?_assertEqual(3, msb_pos(2#0111)),
		?_assertEqual(1, msb_pos(2#0000000000000000000000000000000000000000000000000000000000000001)),
		?_assertEqual(64, msb_pos(2#1000000000000000000000000000000000000000000000000000000000000000)),
		?_assertEqual(57, msb_pos(2#0000000100000001000000010000000100000001000000010000000100000001)),
		?_assertEqual(8, msb_pos(2#0000000000000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(64, msb_pos(2#1111111100000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(64, msb_pos(2#1111111111111111111111111111111111111111111111111111111111111111)),
		?_assertError({badarg, 0}, msb_pos(0)),
		?_assertError({badarg, -1}, msb_pos(-1)),
		?_assertError({badarg, 1.0}, msb_pos(1.0)),
		?_assertError({badarg, 0.0}, msb_pos(0.0))
	].

lsb_pos_test_() ->
	[
		?_assertEqual(1, lsb_pos(1)),
		?_assertEqual(1, lsb_pos(2#01)),
		?_assertEqual(2, lsb_pos(2#10)),
		?_assertEqual(3, lsb_pos(2#100)),
		?_assertEqual(3, lsb_pos(2#0100)),
		?_assertEqual(1, lsb_pos(2#111)),
		?_assertEqual(1, lsb_pos(2#0111)),
		?_assertEqual(4, lsb_pos(2#0101000)),
		?_assertEqual(1, lsb_pos(2#0000000000000000000000000000000000000000000000000000000000000001)),
		?_assertEqual(64, lsb_pos(2#1000000000000000000000000000000000000000000000000000000000000000)),
		?_assertEqual(9, lsb_pos(2#0000000100000001000000010000000100000001000000010000000100000000)),
		?_assertEqual(1, lsb_pos(2#0000000000000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(1, lsb_pos(2#1111111100000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(1, lsb_pos(2#1111111111111111111111111111111111111111111111111111111111111111)),
		?_assertError({badarg, 0}, lsb_pos(0)),
		?_assertError({badarg, -1}, lsb_pos(-1)),
		?_assertError({badarg, 1.0}, lsb_pos(1.0)),
		?_assertError({badarg, 0.0}, lsb_pos(0.0))
	].

ctz_test_() ->
	[
		?_assertEqual(0, ctz(1)),
		?_assertEqual(0, ctz(2#01)),
		?_assertEqual(1, ctz(2#10)),
		?_assertEqual(2, ctz(2#100)),
		?_assertEqual(2, ctz(2#0100)),
		?_assertEqual(0, ctz(2#111)),
		?_assertEqual(0, ctz(2#0111)),
		?_assertEqual(3, ctz(2#0101000)),
		?_assertEqual(0, ctz(2#0000000000000000000000000000000000000000000000000000000000000001)),
		?_assertEqual(63, ctz(2#1000000000000000000000000000000000000000000000000000000000000000)),
		?_assertEqual(8, ctz(2#0000000100000001000000010000000100000001000000010000000100000000)),
		?_assertEqual(0, ctz(2#0000000000000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(0, ctz(2#1111111100000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(0, ctz(2#1111111111111111111111111111111111111111111111111111111111111111)),
		?_assertError({badarg, 0}, ctz(0)),
		?_assertError({badarg, -1}, ctz(-1)),
		?_assertError({badarg, 1.0}, ctz(1.0)),
		?_assertError({badarg, 0.0}, ctz(0.0))
	].

-endif. % end of tests
