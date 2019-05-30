%% MIT License

%% Copyright (c) 2019, Sergei Semichev <chessvegas@chessvegas.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(uef_format).

-export([format_number/3, format_number/4]).
-export([format_price/1, format_price/2, format_price/3]).


%%%------------------------------------------------------------------------------
%%%   EUnit
%%%------------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%------------------------------------------------------------------------------
%%%   Macros
%%%------------------------------------------------------------------------------

-define(DEFAULT_PRICE_PRECISION, 2).
-define(DEFAULT_PRICE_DECIMALS, 2).
-define(THOUSANDS_SEP, <<"">>).
-define(DECIMAL_POINT, <<".">>).
-define(CURRENCY_POSITION, left).
-define(CURRENCY_SEP, <<"">>).


%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type formatted_number() :: binary().
-type precision() :: integer().
-type decimals() :: 0..253. % see types for erlang:float_to_binary/2
-type cur_symbol() :: binary() | [byte()].
-type format_number_opts() :: #{
	thousands_sep => binary() | string(),
	decimal_point => binary() | string(),
	cur_symbol => cur_symbol(),
	cur_pos => left | right,
	cur_sep => binary() | string()
}.

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% format_number/3
-spec format_number(number(), precision(), decimals()) -> formatted_number().
%% @doc
%% The same as uef_format:format_number/4 with #{} as the forth argument. See uef_format:format_number/4 docs.
%% @end
format_number(Number, Precision, Decimals) ->
	format_number(Number, Precision, Decimals, #{}).

%% format_number/4
-spec format_number(number(), precision(), decimals(), format_number_opts()) -> formatted_number().
%% @doc
%% Formats Number by adding thousands separator between each set of 3 digits to the left of the decimal point,
%% substituting Decimals for the decimal point, and rounding to the specified Precision.
%% Returns a binary value.
%% @end
format_number(Number, Precision, Decimals, Opts) when is_integer(Number) ->
	format_number(erlang:float(Number), Precision, Decimals, Opts);
format_number(Number, Precision, Decimals, Opts) when is_float(Number) ->
	Precision2 = case Precision > 0 andalso Decimals < Precision of
		true  -> Decimals;
		false -> Precision
	end,
	RoundedNumber = uef_num:round_number(Number, Precision2), % round to Precision2 before formatting
	do_format_number(RoundedNumber, Decimals, Opts).

%% format_price/1
-spec format_price(Number:: number()) -> FormattedPrice :: formatted_number().
%% @doc
%% Formats Number in price-like style.
%% Returns a binary containing FormattedPrice formatted with a precision of 2 and decimal digits of 2.
%% The same as uef_format:format_price/2 with a precision of 2 as the second argument. See uef_format:format_price/2 docs.
%% @end
format_price(Price) ->
	format_price(Price, ?DEFAULT_PRICE_PRECISION).

%% format_price/2
-spec format_price(Number :: number(), Precision :: precision()) -> FormattedPrice :: formatted_number().
%% @doc
%% Formats Number in price-like style.
%% Returns a binary containing FormattedPrice formatted with a specified precision as the second argument and decimal digits of 2.
%% The same as uef_format:format_price/3 with #{} as the third argument. See uef_format:format_price/3 docs.
%% @end
format_price(Price, Precision) ->
	format_price(Price, Precision, #{}).

%% format_price/3
-spec format_price(Number :: number(), Precision :: precision(), CurrencySymbol_OR_Options :: format_number_opts() | cur_symbol()) -> FormattedPrice :: formatted_number().
%% @doc
%% Formats Number in price-like style.
%% Returns a binary containing FormattedPrice formatted with a specified precision as the second argument, decimal digits of 2,
%% and with currency symbol (or options) as the third argument.
%% If CurrencySymbol_OR_Options is a map the functions works as uef_format:format_number/4
%% with decimal digits of 2 as the third argument and with options as the forth one.
%% If CurrencySymbol_OR_Options is a binary or a string, the corresponding currency symbol is added to the left.
%% @end
format_price(Price, Precision, Opts) when is_map(Opts) ->
	format_number(Price, Precision, ?DEFAULT_PRICE_DECIMALS, Opts);
format_price(Price, Precision, CurSymbol) when is_binary(CurSymbol) orelse is_list(CurSymbol) ->
	format_number(Price, Precision, ?DEFAULT_PRICE_DECIMALS, #{cur_symbol => CurSymbol});
format_price(Price, Precision, Opts) ->
	erlang:error({badarg, Opts}, [Price, Precision, Opts]).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% do_format_number/3
-spec do_format_number(number(), decimals(), format_number_opts()) -> formatted_number().
do_format_number(Number, Decimals, Opts) ->
	PositiveNumber = case Number < 0 of
		false -> Number;
		true  -> erlang:abs(Number)
	end,
	BinNum = erlang:float_to_binary(PositiveNumber, [{decimals, Decimals}]),
	{IntegerPart, DecimalPart} = case uef_bin:split(BinNum, <<".">>) of
		[I, D] -> {I, D}; % ex: <<"12.345">> -> [<<"12">>, <<"345">>]
		[I] -> {I, <<>>} % ex: <<"12345">> -> [<<"12345">>] (when Precision < 1)
	end,
	HeadSize = erlang:byte_size(IntegerPart) rem 3,
	<<Head:HeadSize/binary, IntRest/binary>> = IntegerPart, % ex: <<"12", "345678">> = <<"12345678">>
	ThousandParts = split_thousands(IntRest), % ex: <<"345678">> -> [<<"345">>, <<678>>]
	AllIntegerParts = case HeadSize > 0 of
		true  -> [Head|ThousandParts];
		false -> ThousandParts
	end,
	ThousandsSep = maybe_to_binary(maps:get(thousands_sep, Opts, ?THOUSANDS_SEP)),
	% Join with thousands separator
	FormattedIntegerPart = <<(uef_bin:binary_join(AllIntegerParts, ThousandsSep))/binary>>,
	PositiveFormattedNumber = case DecimalPart of
		<<>> ->
			FormattedIntegerPart;
		_    ->
			DecimalPoint = maybe_to_binary(maps:get(decimal_point, Opts, ?DECIMAL_POINT)),
			<<FormattedIntegerPart/binary, DecimalPoint/binary, DecimalPart/binary>>
	end,
	% Insert "-" before number if negative
	FormattedNumber1 = case Number < 0 of
		false -> PositiveFormattedNumber;
		true  -> <<"-", PositiveFormattedNumber/binary>>
	end,
	% Format with currency options
	format_number_with_currency(FormattedNumber1, Opts).

%% format_number_with_currency/2
-spec format_number_with_currency(binary(), map()) -> binary().
format_number_with_currency(FmtNum, #{cur_symbol := CurSymbol0} = Opts) ->
	CurSymbol = maybe_to_binary(CurSymbol0),
	CurSep = maybe_to_binary(maps:get(cur_sep, Opts, ?CURRENCY_SEP)),
	case maps:get(cur_pos, Opts, ?CURRENCY_POSITION) of
		left -> <<CurSymbol/binary, CurSep/binary, FmtNum/binary>>;
		_ -> <<FmtNum/binary, CurSep/binary, CurSymbol/binary>>
	end;
format_number_with_currency(FmtNum, _) ->
	FmtNum.

%% maybe_to_binary/1
-spec maybe_to_binary(binary() | string()) -> binary().
maybe_to_binary(Sep) ->
	case Sep of
		_ when is_binary(Sep) -> Sep;
		_ when is_list(Sep) -> erlang:list_to_binary(Sep);
		_ -> erlang:error(badarg)
	end.


%% split_thousands/1
-spec split_thousands(binary()) -> [<<_:24>>].
split_thousands(Bin) ->
	split_thousands(Bin, []).

%% split_thousands/2
-spec split_thousands(binary(), [<<_:24>>]) -> [<<_:24>>].
split_thousands(<<>>, List) ->
	lists:reverse(List);
split_thousands(Bin, List) ->
	<<B:3/binary, Rest/binary>> = Bin,
	split_thousands(Rest, [B | List]).


%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

-ifdef(TEST).

format_number_test_() ->
	[
	?_assertEqual(<<"1.00">>, format_number(1, 2, 2, #{})),
	?_assertEqual(<<"1.99">>, format_number(1.99, 2, 2, #{})),
	?_assertEqual(<<"2.00">>, format_number(1.99, 1, 2, #{})),
	?_assertEqual(<<"1 000 999.00">>, format_number(1000999, 2, 2, #{thousands_sep => <<" ">>})),
	?_assertEqual(<<"2,000,000.00">>, format_number(2000000, 2, 2, #{thousands_sep => <<",">>})),
	?_assertEqual(<<"9 999 999 999.00">>, format_number(9999999999, 2, 2, #{thousands_sep => <<" ">>})),
	?_assertEqual(<<"99 999 999 999.99">>, format_price(99999999999.99, 2, #{thousands_sep => <<" ">>})),
	?_assertEqual(<<"999 999 999 999.99">>, format_price(999999999999.99, 2, #{thousands_sep => <<" ">>})),
	?_assertEqual(<<"999,999,999,999.99">>, format_price(999999999999.99,  2, #{thousands_sep => <<",">>})),
	?_assertEqual(<<"USD 1,234,567,890==4600">>, uef_format:format_number(1234567890.4567, 2, 4, #{thousands_sep => ",", decimal_point => "==", cur_symbol => "USD", cur_sep => " ", cur_pos => left})),
	?_assertEqual(<<"$1000.88">>, format_price(1000.8767, 4, "$")),
	?_assertEqual(<<"1000.88 руб."/utf8>>, format_price(1000.8767, 4, #{cur_symbol => <<"руб."/utf8>>, cur_sep => " ", cur_pos => right})),
	?_assertEqual(format_number(100, 2, 3), format_number(100, 2, 3, #{})),
	?_assertEqual(format_price(1000), format_price(1000, 2)),
	?_assertEqual(format_price(1000), format_price(1000, 2, <<>>)),
	?_assertEqual(format_price(1000), format_number(1000, 2, 2, #{}))
	].

-endif. % end of tests
