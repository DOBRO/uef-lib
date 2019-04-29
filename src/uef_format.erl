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
-type decimals() :: non_neg_integer().
-type format_number_opts() :: #{
	thousands_sep => binary() | string(),
	decimal_point => binary() | string(),
	cur_symbol => binary() | string(),
	cur_pos => left | right,
	cur_sep => binary() | string()
}.

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% format_number/3
-spec format_number(number(), precision(), decimals()) -> formatted_number().
format_number(Number, Precision, Decimals) ->
	format_number(Number, Precision, Decimals, #{}).

%% format_number/4
-spec format_number(number(), precision(), decimals(), format_number_opts()) -> formatted_number().
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
-spec format_price(number()) -> formatted_number().
format_price(Price) ->
	format_price(Price, ?DEFAULT_PRICE_PRECISION).

%% format_price/2
-spec format_price(number(), precision()) -> formatted_number().
format_price(Price, Precision) ->
	format_price(Price, Precision, #{}).

%% format_price/3
-spec format_price(number(), precision(), format_number_opts() | binary() | string()) -> formatted_number().
format_price(Price, Precision, Opts) when is_map(Opts) ->
	format_number(Price, Precision, ?DEFAULT_PRICE_DECIMALS, Opts);
format_price(Price, Precision, CurSymbol) when is_binary(CurSymbol) orelse is_list(CurSymbol) ->
	format_number(Price, Precision, ?DEFAULT_PRICE_DECIMALS, #{cur_symbol => CurSymbol});
format_price(_, _, Other) ->
	erlang:error({badarg, Other}).


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
-spec maybe_to_binary(term()) -> binary() | no_return().
maybe_to_binary(Sep) ->
	case Sep of
		_ when is_binary(Sep) -> Sep;
		_ when is_list(Sep) -> erlang:list_to_binary(Sep);
		_ -> erlang:error(badarg)
	end.


%% split_thousands/1
-spec split_thousands(binary()) -> [binary()].
split_thousands(Bin) ->
	split_thousands(Bin, []).

%% split_thousands/2
-spec split_thousands(binary(), [binary()]) -> [binary()].
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
