-module(uef_time).

-export([days_diff/1, days_diff/2]).
-export([seconds_diff/1, seconds_diff/2]).

%% days_diff/1
-spec days_diff(calendar:date()) -> integer().
days_diff(Date) ->
	{DateNow, _} = erlang:localtime(),
	days_diff(DateNow, Date).

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
