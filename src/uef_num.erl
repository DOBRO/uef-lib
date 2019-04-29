-module(uef_num).

-export([round_price/1, round_number/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% round_price/1
-spec round_price(number()) -> float().
round_price(Price) -> round_number(Price, 2).

%% round_number/2
-spec round_number(number(), integer()) -> float().
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
