-module(uef_time).

-export([add_seconds/1, add_seconds/2]).
-export([add_minutes/1, add_minutes/2]).
-export([days_diff/1, days_diff/2]).
-export([seconds_diff/1, seconds_diff/2]).

%%%------------------------------------------------------------------------------
%%%   EUnit
%%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type date() :: calendar:date(). % {Year, Month, Day}
-type datetime() :: calendar:datetime(). % {{Year, Month, Day}, {Hour, Min, Sec}}

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% add_seconds/1
-spec add_seconds(Seconds :: integer()) -> datetime().
add_seconds(Seconds) ->
	add_seconds(erlang:localtime(), Seconds).


%% add_seconds/2
-spec add_seconds(date() | datetime(), integer()) -> datetime().
add_seconds({_Y, _M, _D} = Date, Seconds) ->
	add_seconds({Date, {0, 0, 0}}, Seconds);
add_seconds(DateTime, Seconds) ->
	Seconds2 = calendar:datetime_to_gregorian_seconds(DateTime) + Seconds,
	calendar:gregorian_seconds_to_datetime(Seconds2).

%% add_minutes/1
-spec add_minutes(Minutes :: integer()) -> datetime().
add_minutes(Minutes) ->
	add_seconds(Minutes * 60).


%% add_minutes/2
-spec add_minutes(date() | datetime(), Minutes :: integer()) -> datetime().
add_minutes(DateTime, Minutes) ->
	add_seconds(DateTime, Minutes * 60).


%% days_diff/1
-spec days_diff(date()) -> integer().
days_diff(Date) ->
	days_diff(erlang:date(), Date).

%% days_diff/2
-spec days_diff(date(), date()) -> integer().
days_diff(Date1, Date2) ->
	calendar:date_to_gregorian_days(Date2) - calendar:date_to_gregorian_days(Date1).

%% seconds_diff/1
-spec seconds_diff(datetime()) -> integer().
seconds_diff(DateTime) ->
	DateTimeNow = erlang:localtime(),
	calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds(DateTimeNow).

%% seconds_diff/2
%% Example: seconds_diff({{Year1, Month1, Day1}, {Hour1, Min1, Sec1}}, {{Year2, Month2, Day2}, {Hour2, Min2, Sec2}})
-spec seconds_diff(datetime(), datetime()) -> integer().
seconds_diff(DateTime1, DateTime2) ->
	calendar:datetime_to_gregorian_seconds(DateTime2) - calendar:datetime_to_gregorian_seconds(DateTime1).

%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------



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
