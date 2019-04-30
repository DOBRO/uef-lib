-module(uef_time).

-export([days_diff/1, days_diff/2]).
-export([seconds_diff/1, seconds_diff/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% days_diff/1
-spec days_diff(calendar:date()) -> integer().
days_diff(Date) ->
	days_diff(erlang:date(), Date).

%% days_diff/2
-spec days_diff(calendar:date(), calendar:date()) -> integer().
days_diff(Date1, Date2) ->
	calendar:date_to_gregorian_days(Date2) - calendar:date_to_gregorian_days(Date1).

%% seconds_diff/1
-spec seconds_diff(calendar:datetime()) -> integer().
seconds_diff(DateTime) ->
	DateTimeNow = erlang:localtime(),
	calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds(DateTimeNow).

%% seconds_diff/2
%% Example: seconds_diff({{Year1, Month1, Day1}, {Hour1, Min1, Sec1}}, {{Year2, Month2, Day2}, {Hour2, Min2, Sec2}})
-spec seconds_diff(calendar:datetime(), calendar:datetime()) -> integer().
seconds_diff(DateTime1, DateTime2) ->
	calendar:datetime_to_gregorian_seconds(DateTime2) - calendar:datetime_to_gregorian_seconds(DateTime1).


%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

-ifdef(TEST).

days_diff_2_test_() ->
	[
	?_assertEqual(1, days_diff({2018, 12, 31}, {2019, 1, 1})),
	?_assertEqual(-1, days_diff({2019, 1, 1}, {2018, 12, 31})),
	?_assertEqual(0, days_diff({2019, 4, 23}, {2019, 4, 23}))
	].


seconds_diff_2_test_() ->
	Date = {2019, 4, 23},
	[
	?_assertEqual(0, seconds_diff({Date, {17, 0, 0}}, {Date, {17, 0, 0}})),
	?_assertEqual(1, seconds_diff({Date, {17, 0, 0}}, {Date, {17, 0, 1}})),
	?_assertEqual(3600, seconds_diff({Date, {17, 0, 0}}, {Date, {18, 0, 0}})),
	?_assertEqual(-3600, seconds_diff({Date, {18, 0, 0}}, {Date, {17, 0, 0}})),
	?_assertEqual(60, seconds_diff({Date, {17, 0, 0}}, {Date, {17, 1, 0}}))
	].

-endif. % end of tests
