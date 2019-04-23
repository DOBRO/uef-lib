-module(uef_num_tests).
-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Generator
%%%------------------------------------------------------------------------------

uef_num_test_() ->
	[
		test_round_number()
	].


%%%------------------------------------------------------------------------------
%%%   Test functions
%%%------------------------------------------------------------------------------

test_round_number() ->
	Fun1 = fun uef_num:round_price/1,
	Fun2 = fun uef_num:round_number/2,
	[
		?_assertEqual(1.0, Fun1(1)),
		?_assertEqual(1.01, Fun1(1.01)),
		?_assertEqual(1.01, Fun1(1.015)),
		?_assertEqual(1.02, Fun1(1.025)),
		?_assertEqual(1.02, Fun1(1.0155)),
		?_assertEqual(1.015, Fun2(1.015, 3)),
		?_assertEqual(2.0, Fun2(1.9999, 1)),
		?_assertEqual(2.0, Fun2(1.9999, 2)),
		?_assertEqual(1.9999, Fun2(1.9999, 4)),
		?_assertEqual(-1.9999, Fun2(-1.9999, 4)),
		?_assertEqual(-2.0, Fun2(-1.9999, 3)),
		?_assertEqual(10000.0, Fun2(9999.999999, 5))
	].
