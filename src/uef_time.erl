%% Copyright (c) 2019-2024, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(uef_time).

-export([add_seconds/1, add_seconds/2]).
-export([add_minutes/1, add_minutes/2]).
-export([add_hours/1, add_hours/2]).
-export([add_days/1, add_days/2]).
-export([add_weeks/1, add_weeks/2]).
-export([add_months/1, add_months/2]).
-export([add_years/1, add_years/2]).
-export([add_time/1, add_time/2]).
-export([today/0, tomorrow/0, yesterday/0]).
-export([days_diff/1, days_diff/2]).
-export([seconds_diff/1, seconds_diff/2]).
-export([unix_time/0, unix_time/1]).

%%%------------------------------------------------------------------------------
%%%   Macros
%%%------------------------------------------------------------------------------

-define(UNIX_EPOCH_GREGORIAN_SECONDS, calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})).

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
%% @doc
%% Same as uef_time:add_seconds(erlang:localtime(), Seconds). See docs of uef_time:add_seconds/2.
%% @end
add_seconds(Seconds) ->
    add_seconds(erlang:localtime(), Seconds).

%% add_seconds/2
-spec add_seconds(DateOrDatetime :: date() | datetime(), Seconds :: integer()) -> datetime().
%% @doc
%% Adds the number of seconds Seconds to DateOrDatetime and returns a new datetime value.
%% @end
add_seconds({_Y, _M, _D} = Date, Seconds) ->
    add_seconds({Date, {0, 0, 0}}, Seconds);
add_seconds(DateTime, Seconds) ->
    Seconds2 = calendar:datetime_to_gregorian_seconds(DateTime) + Seconds,
    calendar:gregorian_seconds_to_datetime(Seconds2).


%% add_minutes/1
-spec add_minutes(Minutes :: integer()) -> datetime().
%% @doc
%% Same as uef_time:add_seconds(Minutes * 60). See docs of uef_time:add_seconds/1.
%% @end
add_minutes(Minutes) ->
    add_seconds(Minutes * 60).

%% add_minutes/2
-spec add_minutes(DateOrDatetime :: date() | datetime(), Minutes :: integer()) -> datetime().
%% @doc
%% Adds the number of minutes Minutes to DateOrDatetime and returns a new datetime value.
%% @end
add_minutes(DateOrDatetime, Minutes) ->
    add_seconds(DateOrDatetime, Minutes * 60).


%% add_hours/1
-spec add_hours(Hours :: integer()) -> datetime().
%% @doc
%% Same as uef_time:add_seconds(Hours * 3600). See docs of uef_time:add_seconds/1.
%% @end
add_hours(Hours) ->
    add_seconds(Hours * 3600).

%% add_hours/2
-spec add_hours(DateOrDatetime :: date() | datetime(), Hours :: integer()) -> datetime().
%% @doc
%% Adds the number of hours Hours to DateOrDatetime and returns a new datetime value.
%% @end
add_hours(DateOrDatetime, Hours) ->
    add_seconds(DateOrDatetime, Hours * 3600).


%% add_days/1
-spec add_days(Days :: integer()) -> datetime().
%% @doc
%% Same as uef_time:add_seconds(Days * 86400). See docs of uef_time:add_seconds/1.
%% @end
add_days(Days) ->
    add_seconds(Days * 86400).

%% add_days/2
-spec add_days(DateOrDatetime :: date() | datetime(), Days :: integer()) -> NewDateOrDateTime :: date() | datetime().
%% @doc
%% Adds the number of days Days to DateOrDatetime and returns a new date or datetime value.
%% The type of NewDateOrDateTime is the same as the type of DateOrDatetime.
%% @end
add_days(DateOrDatetime, Days) ->
    {Date, Time} = add_seconds(DateOrDatetime, Days * 86400),
    case DateOrDatetime of
        {_, _} -> {Date, Time}; % type datetime()
        _ -> Date % type date()
    end.

%% add_weeks/1
-spec add_weeks(Weeks :: integer()) -> datetime().
%% @doc
%% Same as uef_time:add_seconds(Weeks * 604800). See docs of uef_time:add_seconds/1.
%% @end
add_weeks(Weeks) ->
    add_seconds(Weeks * 604800).

%% add_weeks/1
-spec add_weeks(DateOrDatetime :: date() | datetime(), Weeks :: integer()) -> NewDateOrDateTime :: date() | datetime().
%% @doc
%% Adds the number of weeks Weeks to DateOrDatetime and returns a new date or datetime value.
%% The type of NewDateOrDateTime is the same as the type of DateOrDatetime.
%% @end
add_weeks(DateOrDatetime, Weeks) ->
    add_days(DateOrDatetime, Weeks * 7).

%% add_months/1
-spec add_months(Months :: integer()) -> datetime().
%% @doc
%% Same as uef_time:add_months(erlang:localtime(), Months). See docs of uef_time:add_months/2.
%% @end
add_months(Months) ->
    add_months(erlang:localtime(), Months).

%% add_months/2
-spec add_months(DateOrDatetime :: date() | datetime(), Months :: integer()) -> NewDateOrDateTime :: date() | datetime().
%% @doc
%% Adds the number of months Months to DateOrDatetime and returns a new date or datetime value.
%% The type of NewDateOrDateTime is the same as the type of DateOrDatetime.
%% @end
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
%% @doc
%% Same as uef_time:add_years(erlang:localtime(), Years). See docs of uef_time:add_years/2.
%% @end
add_years(Years) ->
    add_years(erlang:localtime(), Years).

%% add_years/2
-spec add_years(DateOrDatetime :: date() | datetime(), Years :: integer()) -> NewDateOrDateTime :: date() | datetime().
%% @doc
%% Adds the number of years Years to DateOrDatetime and returns a new date or datetime value.
%% The type of NewDateOrDateTime is the same as the type of DateOrDatetime.
%% @end
add_years(DateOrDatetime, Years) ->
    add_months(DateOrDatetime, Years * 12).


%% add_time/1
-spec add_time(Periods :: periods()) -> datetime().
%% @doc
%% Same as uef_time:add_time(erlang:localtime(), Periods).
%% @end
add_time(Periods) ->
    add_time(erlang:localtime(), Periods).

%% add_time/2
-spec add_time(DateOrDatetime :: date() | datetime(), periods()) -> NewDateOrDateTime :: date() | datetime().
%% @doc
%% Adds one or more periods of time to DateOrDatetime and returns a new date or datetime value.
%% @end
add_time(DT, []) ->
    DT;
add_time(DT, [{N, Ptype} | Tail]) when is_integer(N) andalso is_atom(Ptype) ->
    DT2 = case Ptype of
        % seconds
        sec         -> add_seconds(DT, N);
        second      -> add_seconds(DT, N);
        seconds     -> add_seconds(DT, N);
        % minutes
        min         -> add_minutes(DT, N);
        minute      -> add_minutes(DT, N);
        minutes     -> add_minutes(DT, N);
        % hours
        hrs         -> add_hours(DT, N);
        hour        -> add_hours(DT, N);
        hours       -> add_hours(DT, N);
        % days
        day         -> add_days(DT, N);
        days        -> add_days(DT, N);
        % weeks
        week        -> add_weeks(DT, N);
        weeks       -> add_weeks(DT, N);
        % months
        month       -> add_months(DT, N);
        months      -> add_months(DT, N);
        % years
        year        -> add_years(DT, N);
        years       -> add_years(DT, N);
        % other
        _           -> erlang:error({badarg, Ptype})
    end,
    add_time(DT2, Tail);
add_time(DT, [{Ptype, N} | Tail]) when is_atom(Ptype) andalso is_integer(N) ->
    add_time(DT, [{N, Ptype} | Tail]);
add_time(_, [Arg | _]) ->
    erlang:error({badarg, Arg}).

%% today/0
-spec today() -> CurrentDate :: date().
%% @doc
%% Returns the current date as {Year, Month, Day}. Same as erlang:date(). CurrentDate is of type calendar:date().
%% @end
today() ->
    erlang:date().

%% tomorrow/0
-spec tomorrow() -> TomorrowDate :: date().
%% @doc
%% Returns tomorrow's date as {Year, Month, Day}. TomorrowDate is of type calendar:date().
%% @end
tomorrow() ->
    add_days(erlang:date(), 1).

%% yesterday/0
-spec yesterday() -> YesterdayDate :: date().
%% @doc
%% Returns yesterday's date as {Year, Month, Day}. YesterdayDate is of type calendar:date().
%% @end
yesterday() ->
    add_days(erlang:date(), -1).

%% days_diff/1
-spec days_diff(Date :: date()) -> Days :: integer().
%% @doc
%% Returns the difference in days between Date and the current local date provided by function erlang:date().
%% Date must be of type calendar:date() ({Year, Month, Day}).
%% Days is a positive value if Date is after erlang:date() or a negative value otherwise.
%% @end
days_diff(Date) ->
    days_diff(erlang:date(), Date).

%% days_diff/2
-spec days_diff(Date1 :: date(), Date2 :: date()) -> Days :: integer().
%% @doc
%% Returns the difference in days between Date2 and Date1.
%% Date1 and Date2 must be of type calendar:date() ({Year, Month, Day}).
%% Days is a positive value if Date2 is after Date1 or a negative value otherwise.
%% @end
days_diff(Date1, Date2) ->
    calendar:date_to_gregorian_days(Date2) - calendar:date_to_gregorian_days(Date1).

%% seconds_diff/1
-spec seconds_diff(DateTime :: datetime()) -> Seconds :: integer().
%% @doc
%% Returns the difference in seconds between Date and the current local time provided by function erlang:localtime().
%% DateTime must be of type calendar:datetime() ({{Year, Month, Day}, {Hour, Minute, Second}}).
%% Seconds is a positive value if DateTime is after erlang:localtime() or a negative value otherwise.
%% @end
seconds_diff(DateTime) ->
    DateTimeNow = erlang:localtime(),
    calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds(DateTimeNow).

%% seconds_diff/2
-spec seconds_diff(DateTime1 :: datetime(), DateTime2 :: datetime()) -> integer().
%% @doc
%% Returns the difference in seconds between DateTime2 and DateTime1.
%% DateTime1 and DateTime2 must be of type calendar:datetime() ({{Year, Month, Day}, {Hour, Minute, Second}}).
%% Seconds is a positive value if DateTime2 is after DateTime1 or a negative value otherwise.
%% @end
seconds_diff(DateTime1, DateTime2) ->
    calendar:datetime_to_gregorian_seconds(DateTime2) - calendar:datetime_to_gregorian_seconds(DateTime1).


%% unix_time/0
-spec unix_time() -> Seconds :: integer().
%% @doc
%% Returns the current number of seconds since 00:00:00 (UTC), 1 January 1970. It also known as Unix time or POSIX time or UNIX Epoch time.
%% @end
unix_time() ->
    erlang:system_time(seconds).

%% unix_time/1
-spec unix_time(Datetime :: datetime()) -> Seconds :: integer().
%% @doc
%% Returns the number of seconds elapsed between 00:00:00 (UTC), 1 January 1970 and Datetime. Datetime must be of type calenadr:datetime().
%% @end
unix_time(Datetime) ->
    calendar:datetime_to_gregorian_seconds(Datetime) - ?UNIX_EPOCH_GREGORIAN_SECONDS.


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% validate_datetime/1
-spec validate_datetime(date() | datetime()) -> ok | no_return().
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
