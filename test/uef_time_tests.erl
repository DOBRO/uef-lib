-module(uef_time_tests).
-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Generator
%%%------------------------------------------------------------------------------

uef_time_test_() ->
	[
		test_days_diff_2(),
		test_seconds_diff_2()
	].


%%%------------------------------------------------------------------------------
%%%   Test functions
%%%------------------------------------------------------------------------------

test_days_diff_2() ->
	Fun = fun uef_time:days_diff/2,
	[
		?_assertEqual(1, Fun({2018, 12, 31}, {2019, 1, 1})),
		?_assertEqual(-1, Fun({2019, 1, 1}, {2018, 12, 31})),
		?_assertEqual(0, Fun({2019, 4, 23}, {2019, 4, 23}))
	].


test_seconds_diff_2() ->
	Fun = fun uef_time:seconds_diff/2,
	Date = {2019, 4, 23},
	[
		?_assertEqual(0, Fun({Date, {17, 0, 0}}, {Date, {17, 0, 0}})),
		?_assertEqual(1, Fun({Date, {17, 0, 0}}, {Date, {17, 0, 1}})),
		?_assertEqual(3600, Fun({Date, {17, 0, 0}}, {Date, {18, 0, 0}})),
		?_assertEqual(-3600, Fun({Date, {18, 0, 0}}, {Date, {17, 0, 0}})),
		?_assertEqual(60, Fun({Date, {17, 0, 0}}, {Date, {17, 1, 0}}))
	].
