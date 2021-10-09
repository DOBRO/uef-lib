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

-module(uef_time_tests).

-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

add_seconds_test_() ->
	[
	?_assertEqual({{0,1,1}, {0,0,0}}, uef_time:add_seconds({0, 1, 1}, 0)),
	?_assertEqual({{0,1,1}, {0,0,1}}, uef_time:add_seconds({0, 1, 1}, 1)),
	?_assertEqual({{2001,1,1}, {0,0,0}}, uef_time:add_seconds({{2000,12,31}, {23,59,59}}, 1)),
	?_assertEqual({{2000,2,1}, {0,0,0}}, uef_time:add_seconds({{2000,1,31}, {23,59,59}}, 1)),
	?_assertEqual({{1999,12,31}, {23,59,59}}, uef_time:add_seconds({2000,1,1}, -1)),
	?_assertEqual(uef_time:add_seconds(1), uef_time:add_seconds(erlang:localtime(), 1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_seconds(1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_seconds(erlang:localtime(), 1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_seconds({1,1,1}, 1))
	].

add_minutes_test_() ->
	Date1 = erlang:date(),
	[
	?_assertEqual({{0,1,1}, {0,0,0}}, uef_time:add_minutes({0, 1, 1}, 0)),
	?_assertEqual({{0,1,1}, {0,1,0}}, uef_time:add_minutes({0, 1, 1}, 1)),
	?_assertEqual({{2001,1,1}, {0,0,0}}, uef_time:add_minutes({{2000,12,31}, {23,59,0}}, 1)),
	?_assertEqual({{1999,12,31}, {23,59,0}}, uef_time:add_minutes({2000,1,1}, -1)),
	?_assertEqual(uef_time:add_seconds(Date1, 60), uef_time:add_minutes(Date1, 1)),
	?_assertEqual(uef_time:add_seconds(Date1, -60), uef_time:add_minutes(Date1, -1)),
	?_assertEqual(uef_time:add_minutes(1), uef_time:add_minutes(erlang:localtime(), 1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_minutes(1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_minutes(erlang:localtime(), 1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_minutes(Date1, 1))
	].

add_hours_test_() ->
	Date1 = erlang:date(),
	[
	?_assertEqual({{1999,12,31}, {23,0,0}}, uef_time:add_hours({2000,1,1}, -1)),
	?_assertEqual({{2001,1,1}, {0,0,0}}, uef_time:add_hours({{2000,12,31}, {23,0,0}}, 1)),
	?_assertEqual(uef_time:add_hours(1), uef_time:add_hours(erlang:localtime(), 1)),
	?_assertEqual(uef_time:add_seconds(Date1, 3600), uef_time:add_hours(Date1, 1)),
	?_assertEqual(uef_time:add_seconds(Date1, -3600), uef_time:add_hours(Date1, -1)),
	?_assertEqual(uef_time:add_minutes(Date1, 60), uef_time:add_hours(Date1, 1)),
	?_assertEqual(uef_time:add_minutes(Date1, -60), uef_time:add_hours(Date1, -1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_hours(1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_hours(erlang:localtime(), 1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_hours(Date1, 1))
	].

add_days_test_() ->
	Date1 = erlang:date(),
	[
	?_assertEqual({1999,12,31}, uef_time:add_days({2000,1,1}, -1)),
	?_assertEqual({{2001,1,1}, {0,0,0}}, uef_time:add_days({{2000,12,31}, {0,0,0}}, 1)),
	?_assertEqual(uef_time:add_days(1), uef_time:add_days(erlang:localtime(), 1)),
	?_assertEqual(uef_time:add_seconds(Date1, 86400), uef_time:add_days({Date1, {0,0,0}}, 1)),
	?_assertEqual(uef_time:add_seconds(Date1, -86400), uef_time:add_days({Date1, {0,0,0}}, -1)),
	?_assertEqual(uef_time:add_hours(Date1, 24), uef_time:add_days({Date1, {0,0,0}}, 1)),
	?_assertEqual(uef_time:add_hours(Date1, -24), uef_time:add_days({Date1, {0,0,0}}, -1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_days(1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_days(erlang:localtime(), 1)),
	?_assertMatch({_,_,_}, uef_time:add_days(Date1, 1))
	].

add_weeks_test_() ->
	Date1 = erlang:date(),
	[
	?_assertEqual(uef_time:add_days(7), uef_time:add_weeks(1)),
	?_assertEqual(uef_time:add_days(-7), uef_time:add_weeks(-1)),
	?_assertEqual(uef_time:add_days(Date1, 7), uef_time:add_weeks(Date1, 1)),
	?_assertEqual(uef_time:add_days(Date1, -7), uef_time:add_weeks(Date1, -1)),
	?_assertEqual({2019, 1, 8}, uef_time:add_weeks({2019, 1, 1}, 1)),
	?_assertEqual({2018, 12, 25}, uef_time:add_weeks({2019, 1, 1}, -1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_weeks(1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_weeks(erlang:localtime(), 1)),
	?_assertMatch({_,_,_}, uef_time:add_weeks(Date1, 1))
	].

add_months_test_() ->
	Date1 = erlang:date(),
	{Year1, _, _} = Date1,
	[
	?_assertEqual({2016, 2, 29}, uef_time:add_months({2016, 1, 31}, 1)), % leap year
	?_assertEqual({2017, 2, 28}, uef_time:add_months({2017, 1, 31}, 1)), % non-leap year
	?_assertEqual({Year1 - 1, 12, 31}, uef_time:add_months({Year1, 1, 31}, -1)),
	?_assertEqual({Year1 + 1, 1, 31}, uef_time:add_months({Year1, 1, 31}, 12)),
	?_assertEqual({Year1 - 1, 1, 31}, uef_time:add_months({Year1, 1, 31}, -12)),
	?_assertEqual({Year1, 2, 1}, uef_time:add_months({Year1, 1, 1}, 1)),
	?_assertEqual({Year1 - 1, 12, 1}, uef_time:add_months({Year1, 1, 1}, -1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_months(1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_months(erlang:localtime(), 1)),
	?_assertMatch({_,_,_}, uef_time:add_months(Date1, 1))
	].

add_years_test_() ->
	Date1 = erlang:date(),
	{Year1, _, _} = Date1,
	[
	?_assertEqual(uef_time:add_months(12), uef_time:add_years(1)),
	?_assertEqual(uef_time:add_months(-12), uef_time:add_years(-1)),
	?_assertEqual(uef_time:add_months(Date1, 12), uef_time:add_years(Date1, 1)),
	?_assertEqual(uef_time:add_months(Date1, -12), uef_time:add_years(Date1, -1)),
	?_assertEqual({Year1 + 1, 1, 1}, uef_time:add_years({Year1, 1, 1}, 1)),
	?_assertEqual({Year1 - 1, 1, 1}, uef_time:add_years({Year1, 1, 1}, -1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_years(1)),
	?_assertMatch({{_,_,_}, {_,_,_}}, uef_time:add_years(erlang:localtime(), 1)),
	?_assertMatch({_,_,_}, uef_time:add_years(Date1, 1))
	].

add_time_test_() ->
	Date1 = erlang:date(),
	DateTime1 = erlang:localtime(),
	AllPeriods = [{1, year}, {1, month}, {1, week}, {1, day}, {1, hour}, {1, minute}, {1, second}],
	[
	?_assertEqual(uef_time:add_time(AllPeriods), uef_time:add_time(erlang:localtime(), AllPeriods)),

	?_assertEqual(uef_time:add_time(Date1, [{1, sec}]), uef_time:add_time(Date1, [{1, seconds}])),
	?_assertEqual(uef_time:add_time(Date1, [{1, second}]), uef_time:add_time(Date1, [{1, seconds}])),
	?_assertEqual(uef_time:add_time(Date1, [{1, min}]), uef_time:add_time(Date1, [{1, minutes}])),
	?_assertEqual(uef_time:add_time(Date1, [{1, minute}]), uef_time:add_time(Date1, [{1, minutes}])),
	?_assertEqual(uef_time:add_time(Date1, [{1, hrs}]), uef_time:add_time(Date1, [{1, hours}])),
	?_assertEqual(uef_time:add_time(Date1, [{1, hour}]), uef_time:add_time(Date1, [{1, hours}])),
	?_assertEqual(uef_time:add_time(Date1, [{1, day}]), uef_time:add_time(Date1, [{1, days}])),
	?_assertEqual(uef_time:add_time(Date1, [{1, week}]), uef_time:add_time(Date1, [{1, weeks}])),
	?_assertEqual(uef_time:add_time(Date1, [{1, month}]), uef_time:add_time(Date1, [{1, months}])),
	?_assertEqual(uef_time:add_time(Date1, [{1, year}]), uef_time:add_time(Date1, [{1, years}])),

	?_assertEqual(uef_time:add_time(Date1, [{seconds, 1}]), uef_time:add_time(Date1, [{1, seconds}])),
	?_assertEqual(uef_time:add_time(Date1, [{minutes, 1}]), uef_time:add_time(Date1, [{1, minutes}])),
	?_assertEqual(uef_time:add_time(Date1, [{hours, 1}]), uef_time:add_time(Date1, [{1, hours}])),
	?_assertEqual(uef_time:add_time(Date1, [{days, 1}]), uef_time:add_time(Date1, [{1, days}])),
	?_assertEqual(uef_time:add_time(Date1, [{weeks, 1}]), uef_time:add_time(Date1, [{1, weeks}])),
	?_assertEqual(uef_time:add_time(Date1, [{months, 1}]), uef_time:add_time(Date1, [{1, months}])),
	?_assertEqual(uef_time:add_time(Date1, [{years, 1}]), uef_time:add_time(Date1, [{1, years}])),

	?_assertEqual(uef_time:add_seconds(Date1, 1), uef_time:add_time(Date1, [{1, seconds}])),
	?_assertEqual(uef_time:add_seconds(DateTime1, 1), uef_time:add_time(DateTime1, [{1, seconds}])),
	?_assertEqual(uef_time:add_minutes(Date1, 1), uef_time:add_time(Date1, [{1, minutes}])),
	?_assertEqual(uef_time:add_minutes(DateTime1, 1), uef_time:add_time(DateTime1, [{1, minutes}])),
	?_assertEqual(uef_time:add_hours(Date1, 1), uef_time:add_time(Date1, [{1, hours}])),
	?_assertEqual(uef_time:add_hours(DateTime1, 1), uef_time:add_time(DateTime1, [{1, hours}])),
	?_assertEqual(uef_time:add_days(Date1, 1), uef_time:add_time(Date1, [{1, days}])),
	?_assertEqual(uef_time:add_days(DateTime1, 1), uef_time:add_time(DateTime1, [{1, days}])),
	?_assertEqual(uef_time:add_weeks(Date1, 1), uef_time:add_time(Date1, [{1, weeks}])),
	?_assertEqual(uef_time:add_weeks(DateTime1, 1), uef_time:add_time(DateTime1, [{1, weeks}])),
	?_assertEqual(uef_time:add_months(Date1, 1), uef_time:add_time(Date1, [{1, months}])),
	?_assertEqual(uef_time:add_months(DateTime1, 1), uef_time:add_time(DateTime1, [{1, months}])),
	?_assertEqual(uef_time:add_years(Date1, 1), uef_time:add_time(Date1, [{1, years}])),
	?_assertEqual(uef_time:add_years(DateTime1, 1), uef_time:add_time(DateTime1, [{1, years}])),

	?_assertException(error, {badarg, some_period}, uef_time:add_time(DateTime1, [{1, some_period}])),
	?_assertException(error, {badarg, {1.0, year}}, uef_time:add_time(DateTime1, [{1.0, year}]))
	].

today_test_() ->
	[
	?_assertEqual(erlang:date(), uef_time:today()),
	?_assertMatch({_,_,_}, uef_time:today())
	].

tomorrow_test_() ->
	[
	?_assertEqual(uef_time:add_days(uef_time:today(), 1), uef_time:tomorrow()),
	?_assertMatch({_,_,_}, uef_time:tomorrow())
	].

yesterday_test_() ->
	[
	?_assertEqual(uef_time:add_days(uef_time:today(), -1), uef_time:yesterday()),
	?_assertMatch({_,_,_}, uef_time:yesterday())
	].

days_diff_1_test_() ->
	Date = {2021, 12, 31},
	[
	?_assertEqual(uef_time:days_diff(Date), uef_time:days_diff(erlang:date(), Date))
	].

days_diff_2_test_() ->
	[
	?_assertEqual(1, uef_time:days_diff({2018, 12, 31}, {2019, 1, 1})),
	?_assertEqual(-1, uef_time:days_diff({2019, 1, 1}, {2018, 12, 31})),
	?_assertEqual(0, uef_time:days_diff({2019, 4, 23}, {2019, 4, 23}))
	].

seconds_diff_1_test_() ->
	DateTime = {{2021, 12, 31}, {0, 0, 0}},
	[
	?_assertEqual(uef_time:seconds_diff(DateTime), uef_time:seconds_diff(erlang:localtime(), DateTime))
	].

seconds_diff_2_test_() ->
	Date = erlang:date(),
	[
	?_assertEqual(0, uef_time:seconds_diff({Date, {17, 0, 0}}, {Date, {17, 0, 0}})),
	?_assertEqual(1, uef_time:seconds_diff({Date, {17, 0, 0}}, {Date, {17, 0, 1}})),
	?_assertEqual(3600, uef_time:seconds_diff({Date, {17, 0, 0}}, {Date, {18, 0, 0}})),
	?_assertEqual(-3600, uef_time:seconds_diff({Date, {18, 0, 0}}, {Date, {17, 0, 0}})),
	?_assertEqual(60, uef_time:seconds_diff({Date, {17, 0, 0}}, {Date, {17, 1, 0}}))
	].

unix_time_test_() ->
	[
	?_assertEqual(0, uef_time:unix_time({{1970,1,1}, {0,0,0}})),
	?_assertEqual(1, uef_time:unix_time({{1970,1,1}, {0,0,1}})),
	?_assertEqual(59, uef_time:unix_time({{1970,1,1}, {0,0,59}})),
	?_assertEqual(uef_time:unix_time(), uef_time:unix_time(calendar:universal_time()))
	].
