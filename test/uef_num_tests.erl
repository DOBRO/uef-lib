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

-module(uef_num_tests).

-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

round_number_test_() ->
	[
	?_assertEqual(1.0, uef_num:round_price(1)),
	?_assertEqual(1.01, uef_num:round_price(1.01)),
	?_assertEqual(1.01, uef_num:round_price(1.015)),
	?_assertEqual(1.02, uef_num:round_price(1.025)),
	?_assertEqual(1.02, uef_num:round_price(1.0155)),
	?_assertEqual(1.015, uef_num:round_number(1.015, 3)),
	?_assertEqual(2.0, uef_num:round_number(1.9999, 1)),
	?_assertEqual(2.0, uef_num:round_number(1.9999, 2)),
	?_assertEqual(1.9999, uef_num:round_number(1.9999, 4)),
	?_assertEqual(-1.9999, uef_num:round_number(-1.9999, 4)),
	?_assertEqual(-2.0, uef_num:round_number(-1.9999, 3)),
	?_assertEqual(10000.0, uef_num:round_number(9999.999999, 5))
	].


popcount_test_() ->
	[
		?_assertEqual(0, uef_num:popcount(0)),
		?_assertEqual(3, uef_num:popcount(7)),
		?_assertEqual(1, uef_num:popcount(8)),
		?_assertEqual(1, uef_num:popcount(8)),
		?_assertEqual(1, uef_num:popcount(2#0000000000000000000000000000000000000000000000000000000000000001)),
		?_assertEqual(1, uef_num:popcount(2#1000000000000000000000000000000000000000000000000000000000000000)),
		?_assertEqual(8, uef_num:popcount(2#0000000100000001000000010000000100000001000000010000000100000001)),
		?_assertEqual(8, uef_num:popcount(2#0000000000000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(16, uef_num:popcount(2#1111111100000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(64, uef_num:popcount(2#1111111111111111111111111111111111111111111111111111111111111111)),
		?_assertError({badarg, -1}, uef_num:popcount(-1)),
		?_assertError({badarg, 1.0}, uef_num:popcount(1.0)),
		?_assertError({badarg, 0.0}, uef_num:popcount(0.0))
	].


msb_pos_test_() ->
	[
		?_assertEqual(1, uef_num:msb_pos(2#1)),
		?_assertEqual(1, uef_num:msb_pos(2#01)),
		?_assertEqual(2, uef_num:msb_pos(2#10)),
		?_assertEqual(3, uef_num:msb_pos(2#100)),
		?_assertEqual(3, uef_num:msb_pos(2#0100)),
		?_assertEqual(3, uef_num:msb_pos(2#111)),
		?_assertEqual(3, uef_num:msb_pos(2#0111)),
		?_assertEqual(1, uef_num:msb_pos(2#0000000000000000000000000000000000000000000000000000000000000001)),
		?_assertEqual(64, uef_num:msb_pos(2#1000000000000000000000000000000000000000000000000000000000000000)),
		?_assertEqual(57, uef_num:msb_pos(2#0000000100000001000000010000000100000001000000010000000100000001)),
		?_assertEqual(8, uef_num:msb_pos(2#0000000000000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(64, uef_num:msb_pos(2#1111111100000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(64, uef_num:msb_pos(2#1111111111111111111111111111111111111111111111111111111111111111)),
		?_assertError({badarg, 0}, uef_num:msb_pos(0)),
		?_assertError({badarg, -1}, uef_num:msb_pos(-1)),
		?_assertError({badarg, 1.0}, uef_num:msb_pos(1.0)),
		?_assertError({badarg, 0.0}, uef_num:msb_pos(0.0))
	].

lsb_pos_test_() ->
	[
		?_assertEqual(1, uef_num:lsb_pos(1)),
		?_assertEqual(1, uef_num:lsb_pos(2#01)),
		?_assertEqual(2, uef_num:lsb_pos(2#10)),
		?_assertEqual(3, uef_num:lsb_pos(2#100)),
		?_assertEqual(3, uef_num:lsb_pos(2#0100)),
		?_assertEqual(1, uef_num:lsb_pos(2#111)),
		?_assertEqual(1, uef_num:lsb_pos(2#0111)),
		?_assertEqual(4, uef_num:lsb_pos(2#0101000)),
		?_assertEqual(1, uef_num:lsb_pos(2#0000000000000000000000000000000000000000000000000000000000000001)),
		?_assertEqual(64, uef_num:lsb_pos(2#1000000000000000000000000000000000000000000000000000000000000000)),
		?_assertEqual(9, uef_num:lsb_pos(2#0000000100000001000000010000000100000001000000010000000100000000)),
		?_assertEqual(1, uef_num:lsb_pos(2#0000000000000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(1, uef_num:lsb_pos(2#1111111100000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(1, uef_num:lsb_pos(2#1111111111111111111111111111111111111111111111111111111111111111)),
		?_assertError({badarg, 0}, uef_num:lsb_pos(0)),
		?_assertError({badarg, -1}, uef_num:lsb_pos(-1)),
		?_assertError({badarg, 1.0}, uef_num:lsb_pos(1.0)),
		?_assertError({badarg, 0.0}, uef_num:lsb_pos(0.0))
	].

ctz_test_() ->
	[
		?_assertEqual(0, uef_num:ctz(1)),
		?_assertEqual(0, uef_num:ctz(2#01)),
		?_assertEqual(1, uef_num:ctz(2#10)),
		?_assertEqual(2, uef_num:ctz(2#100)),
		?_assertEqual(2, uef_num:ctz(2#0100)),
		?_assertEqual(0, uef_num:ctz(2#111)),
		?_assertEqual(0, uef_num:ctz(2#0111)),
		?_assertEqual(3, uef_num:ctz(2#0101000)),
		?_assertEqual(0, uef_num:ctz(2#0000000000000000000000000000000000000000000000000000000000000001)),
		?_assertEqual(63, uef_num:ctz(2#1000000000000000000000000000000000000000000000000000000000000000)),
		?_assertEqual(8, uef_num:ctz(2#0000000100000001000000010000000100000001000000010000000100000000)),
		?_assertEqual(0, uef_num:ctz(2#0000000000000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(0, uef_num:ctz(2#1111111100000000000000000000000000000000000000000000000011111111)),
		?_assertEqual(0, uef_num:ctz(2#1111111111111111111111111111111111111111111111111111111111111111)),
		?_assertError({badarg, 0}, uef_num:ctz(0)),
		?_assertError({badarg, -1}, uef_num:ctz(-1)),
		?_assertError({badarg, 1.0}, uef_num:ctz(1.0)),
		?_assertError({badarg, 0.0}, uef_num:ctz(0.0))
	].
