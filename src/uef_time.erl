-module(uef_time).

-export([add_seconds/1, add_seconds/2]).
-export([add_minutes/1, add_minutes/2]).
-export([add_hours/1, add_hours/2]).
-export([add_days/1, add_days/2]).
-export([add_weeks/1, add_weeks/2]).
-export([add_months/1, add_months/2]).
-export([add_years/1, add_years/2]).
-export([add_time/1, add_time/2]).
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
-type psecond() :: sec | second | seconds.
-type pminute() :: min | minute | minutes.
-type phour() :: hrs | hour | hours.
-type pday() :: day | days.
-type pmonth() :: month | months.
-type pyear() :: year | years.
-type ptype() :: psecond() | pminute() | phour() | pday() | pmonth() | pyear().
-type period() :: {integer(), ptype()} | {ptype(), integer()}.
-type periods() :: [period()].

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
add_minutes(DateOrDatetime, Minutes) ->
	add_seconds(DateOrDatetime, Minutes * 60).


%% add_hours/1
-spec add_hours(Hours :: integer()) -> datetime().
add_hours(Hours) ->
	add_seconds(Hours * 3600).

%% add_hours/2
-spec add_hours(date() | datetime(), Hours :: integer()) -> datetime().
add_hours(DateOrDatetime, Hours) ->
	add_seconds(DateOrDatetime, Hours * 3600).


%% add_days/1
-spec add_days(Days :: integer()) -> datetime().
add_days(Days) ->
	add_seconds(Days * 86400).

%% add_days/2
-spec add_days(date() | datetime(), Days :: integer()) -> date() | datetime().
add_days(DateOrDatetime, Days) ->
	{Date, Time} = add_seconds(DateOrDatetime, Days * 86400),
	case DateOrDatetime of
		{_, _} -> {Date, Time}; % type datetime()
		_ -> Date % type date()
	end.

%% add_weeks/1
-spec add_weeks(Weeks :: integer()) -> datetime().
add_weeks(Weeks) ->
	add_seconds(Weeks * 604800).

%% add_weeks/1
-spec add_weeks(date() | datetime(), Weeks :: integer()) -> date() | datetime().
add_weeks(DateOrDatetime, Weeks) ->
	add_days(DateOrDatetime, Weeks * 7).

%% add_months/1
-spec add_months(integer()) -> datetime().
add_months(Months) ->
	add_months(erlang:localtime(), Months).

%% add_months/2
-spec add_months(date() | datetime(), Months :: integer()) -> date() | datetime().
add_months(DateOrDatetime, Months) ->
	ok = validate_datetime(DateOrDatetime),
	{Year1, Mon1, Day1, Time} = case DateOrDatetime of
		{ {Y, M, D}, T} -> {Y, M, D, T};
		{Y, M, D} -> {Y, M, D, skip}
	end,
	TotalMonths = Year1 * 12 + Mon1 + Months,
	{Year2, Mon2} = case TotalMonths rem 12 of
		0 -> {(TotalMonths div 12) - 1, 12};
		N -> {TotalMonths div 12, N}
	end,
	Date = add_months_check_date(Year2, Mon2, Day1),
	case Time of
		skip -> Date;
		_ -> {Date, Time}
	end.


%% add_years/1
-spec add_years(Years :: integer()) -> datetime().
add_years(Years) ->
	add_years(erlang:localtime(), Years).

%% add_years/2
-spec add_years(date() | datetime(), Years :: integer()) -> datetime().
add_years(DateOrDatetime, Years) ->
	add_months(DateOrDatetime, Years * 12).


%% add_time/1
-spec add_time(periods()) -> datetime().
add_time(Periods) ->
	add_time(erlang:localtime(), Periods).

%% add_time/2
-spec add_time(date() | datetime(), periods()) -> date() | datetime().
add_time(DT, []) ->
	DT;
add_time(DT, [{N, Ptype} | Tail]) when is_integer(N) andalso is_atom(Ptype) ->
	DT2 = case Ptype of
		% seconds
		sec			-> add_seconds(DT, N);
		second		-> add_seconds(DT, N);
		seconds		-> add_seconds(DT, N);
		% minutes
		min			-> add_minutes(DT, N);
		minute		-> add_minutes(DT, N);
		minutes		-> add_minutes(DT, N);
		% hours
		hrs			-> add_hours(DT, N);
		hour		-> add_hours(DT, N);
		hours		-> add_hours(DT, N);
		% days
		day			-> add_days(DT, N);
		days		-> add_days(DT, N);
		% weeks
		week		-> add_weeks(DT, N);
		weeks		-> add_weeks(DT, N);
		% months
		month		-> add_months(DT, N);
		months		-> add_months(DT, N);
		% years
		year		-> add_years(DT, N);
		years		-> add_years(DT, N);
		% other
		_			-> erlang:error({badarg, Ptype})
	end,
	add_time(DT2, Tail);
add_time(DT, [{Ptype, N} | Tail]) when is_atom(Ptype) andalso is_integer(N) ->
	add_time(DT, [{N, Ptype} | Tail]);
add_time(_, [Arg | _]) ->
	erlang:error({badarg, Arg}).


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

%% validate_datetime/1
-spec validate_datetime(date() | datetime()) -> ok | no_return.
validate_datetime({_, _, _} = Date) ->
	validate_datetime({Date, {0, 0, 0}});
validate_datetime({Date, {Hour, Min, Sec}}) ->
	true = calendar:valid_date(Date),
	true = erlang:is_integer(Hour) andalso Hour > -1 andalso Hour < 24,
	true = erlang:is_integer(Min) andalso Min > -1 andalso Min < 60,
	true = erlang:is_integer(Sec) andalso Sec > -1 andalso Sec < 60,
	ok;
validate_datetime(Other) ->
	erlang:error({badarg, Other}).


%% add_months_check_date/3
-spec add_months_check_date(integer(), integer(), integer()) -> date() | no_return().
add_months_check_date(Y, M, D) when D > 0 ->
	case calendar:valid_date(Y, M, D) of
		true  ->
			{Y, M, D};
		false ->
			add_months_check_date(Y, M, D - 1)
	end;
add_months_check_date(Y, M, D) -> % just in case
	erlang:error({out_of_range, {Y, M, D}}).


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
