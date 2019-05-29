%% MIT License

%% Copyright (c) 2019, Sergei Semichev <chessvegas@chessvegas.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(uef_num).

-export([round_price/1, round_number/2]).

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

-endif. % end of tests
