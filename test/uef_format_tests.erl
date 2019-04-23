-module(uef_format_tests).
-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Generator
%%%------------------------------------------------------------------------------

uef_format_test_() ->
	[
		test_format_price()
	].


%%%------------------------------------------------------------------------------
%%%   Test functions
%%%------------------------------------------------------------------------------

test_format_price() ->
	Fun1 = fun uef_format:format_price/1,
	Fun2 = fun uef_format:format_price/2,
	[
		?_assertEqual(<<"1.00">>, Fun1(1)),
		?_assertEqual(<<"1.99">>, Fun1(1.99)),
		?_assertEqual(<<"100.00">>, Fun1(100)),
		?_assertEqual(<<"102.00">>, Fun1(101.9999)),
		?_assertEqual(<<"-102.00">>, Fun1(-101.9999)),
		?_assertEqual(<<"-103.00">>, Fun1(-102.9999)),
		?_assertEqual(<<"-100.00">>, Fun1(-100)),
		?_assertEqual(<<"1 000.00">>, Fun1(1000)),
		?_assertEqual(<<"-1 000.00">>, Fun1(-1000)),
		?_assertEqual(<<"1 000 999.00">>, Fun1(1000999)),
		?_assertEqual(<<"2 000 000.00">>, Fun1(1999999.999)),
		?_assertEqual(<<"9 999 999 999.00">>, Fun1(9999999999)),
		?_assertEqual(<<"99 999 999 999.99">>, Fun1(99999999999.99)),
		?_assertEqual(<<"999 999 999 999.99">>, Fun1(999999999999.99)),
		?_assertEqual(<<"999,999,999,999.99">>, Fun2(999999999999.99, <<",">>))
	].
