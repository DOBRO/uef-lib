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
-export([format_bytes/1, format_bytes/2]).

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
-define(MULTI_BYTE_UNITS, ['KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB']).


%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type formatted_number() :: binary().
-type precision() :: integer().
-type decimals() :: 0..253. % see types for erlang:float_to_binary/2
-type cur_symbol() :: binary() | string().
-type format_number_opts() :: #{
	thousands_sep => binary() | string(),
	decimal_point => binary() | string(),
	cur_symbol => cur_symbol(),
	cur_pos => left | right,
	cur_sep => binary() | string()
}.
-type multi_byte_unit() :: 'KB' | 'MB' | 'GB' | 'TB' | 'PB' | 'EB' | 'ZB' | 'YB'.
-type byte_opts_in() :: #{
	base  => 2 | 10,
	units => auto | multi_byte_unit(),
	to_type  => bin | int,
	sep => binary()
}.
-type valid_byte_opts() :: #{
	base  => 1000 | 1024,
	units => auto | multi_byte_unit(),
	to_type  => bin | int,
	sep => binary()
}.
-type formatted_bytes() :: binary() | integer() | {integer(), multi_byte_unit()}.
-type byte_opts_error() :: {invalid_base|invalid_units|invalid_output_type|invalid_separator, term()}.

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


%% format_bytes/1
-spec format_bytes(integer()) -> formatted_bytes().
%% @equiv format_bytes(Bytes, #{})
format_bytes(Bytes) ->
	format_bytes(Bytes, #{}).

%% format_bytes/2
%% @doc
%% Converts bytes to multiples of bytes (KB, MB, GB, TB, PB, EB, ZB, YB).
%% See README for details.
%% @end
-spec format_bytes(integer(), byte_opts_in()) -> formatted_bytes().
format_bytes(Bytes, Opts0) when is_integer(Bytes), is_map(Opts0) ->
	case validate_byte_opts(Opts0) of
		{ok, Opts} ->
			do_format_bytes(Bytes, Opts);
		{error, Reason} ->
			erlang:error(Reason, [Bytes, Opts0])
	end;
format_bytes(Bytes, Opts) ->
	BadArg = case is_integer(Bytes) of
		true  -> Opts;
		false -> Bytes
	end,
	erlang:error({badarg, BadArg},  [Bytes, Opts]).


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
maybe_to_binary(B) when is_binary(B) ->
	B;
maybe_to_binary(L) when is_list(L) ->
	case unicode:characters_to_binary(L, utf8, utf8) of
		B when is_binary(B) -> B;
		_ -> erlang:error(badarg)
	end;
maybe_to_binary(_)->
	erlang:error(badarg).


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


%% do_format_bytes/2
-spec do_format_bytes(integer(), valid_byte_opts()) -> formatted_bytes().
do_format_bytes(Bytes, Opts) ->
	#{base := Base, units := Units0, to_type := Type, sep := Sep} = Opts,
	{MultiBytes, Units} = bytes_to_multiple(Bytes, Units0, Base),
	case Type of
		bin ->
			BinMultiBytes = erlang:integer_to_binary(MultiBytes),
			BinUnits = erlang:atom_to_binary(Units, latin1),
			<<BinMultiBytes/bits, Sep/bits , BinUnits/bits>>;
		int when (Units0 =:= auto) ->
			{MultiBytes, Units};
		int ->
			MultiBytes
	end.

%% bytes_to_multiple/3
-spec bytes_to_multiple(integer(), auto | multi_byte_unit(), 1000 | 1024) -> {integer(), multi_byte_unit()}.
bytes_to_multiple(Bytes, Units, Base) ->
	bytes_to_multiple(Bytes, Units, Base, ?MULTI_BYTE_UNITS, 0, 'KB', 1).


%% bytes_to_multiple/7
-spec bytes_to_multiple(integer(), auto | multi_byte_unit(), 1000 | 1024, [multi_byte_unit()], integer(), multi_byte_unit(), pos_integer()) ->
	{integer(), multi_byte_unit()}.
bytes_to_multiple(_Bytes, _Units0, _Base, [], MultiBytes, Units, _Pow) ->
	{MultiBytes, Units};
bytes_to_multiple(Bytes, Units0, Base, [CurUnits | Tail], MultiBytesBefore, UnitsBefore, Pow) ->
	MultiBytes = erlang:trunc(Bytes/math:pow(Base, Pow)),
	case Units0 of
		CurUnits ->
			{MultiBytes, Units0};
		auto when (MultiBytes =:= 0) ->
			{MultiBytesBefore, UnitsBefore};
		_ ->
			bytes_to_multiple(Bytes, Units0, Base, Tail, MultiBytes, CurUnits, Pow + 1)
	end.


%% validate_byte_opts/1
-spec validate_byte_opts(byte_opts_in()) -> {ok, valid_byte_opts()} | {error, byte_opts_error()}.
validate_byte_opts(Opts0) ->
	validate_byte_opts([base, units, to_type, sep], Opts0, #{}).

%% validate_byte_opts/2
-spec validate_byte_opts([base|units|to_type|sep,...], byte_opts_in(), valid_byte_opts()) -> {ok, valid_byte_opts()} | {error, byte_opts_error()}.
validate_byte_opts([], _Opts0, Acc) ->
	{ok, Acc};
validate_byte_opts([base|Tail], Opts0, Acc) -> % base
	case maps:find(base, Opts0) of
		error    -> validate_byte_opts(Tail, Opts0, Acc#{base => 1024}); % default
		{ok, 2}  -> validate_byte_opts(Tail, Opts0, Acc#{base => 1024});
		{ok, 10} -> validate_byte_opts(Tail, Opts0, Acc#{base => 1000});
		{ok, Base} -> {error, {invalid_base, Base}}
	end;
validate_byte_opts([units|Tail], Opts0, Acc) -> % units
	case maps:find(units, Opts0) of
		error -> % Units not specified, set them to 'auto'
			validate_byte_opts(Tail, Opts0, Acc#{units => auto});
		{ok, auto} -> % auto
			validate_byte_opts(Tail, Opts0, Acc#{units => auto});
		{ok, Units} -> % Units specified, check them
			case lists:member(Units, ?MULTI_BYTE_UNITS) of
				true -> validate_byte_opts(Tail, Opts0, Acc#{units => Units});
				false -> {error, {invalid_units, Units}}
			end
	end;
validate_byte_opts([to_type|Tail], Opts0, Acc) -> % to_type
	case maps:find(to_type, Opts0) of
		error -> % Output type not specified, set it to 'bin'
			validate_byte_opts(Tail, Opts0, Acc#{to_type => 'bin'});
		{ok, Type} when (Type =:= int) orelse (Type =:= bin) ->
			validate_byte_opts(Tail, Opts0, Acc#{to_type => Type});
		{ok, Type} ->
			{error, {invalid_output_type, Type}}
	end;
validate_byte_opts([sep|Tail], Opts0, Acc) -> % separator
	case maps:find(sep, Opts0) of
		error -> % Separator not specified, set it to <<>> (empty binary)
			validate_byte_opts(Tail, Opts0, Acc#{sep => <<>>});
		{ok, Sep} when is_binary(Sep) ->
			validate_byte_opts(Tail, Opts0, Acc#{sep => Sep});
		{ok, Sep} ->
			{error, {invalid_separator, Sep}}
	end.


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
	?_assertEqual(<<"1000.88 руб."/utf8>>, format_price(1000.8767, 4, #{cur_symbol => "руб.", cur_sep => " ", cur_pos => right})),
	?_assertEqual(<<"€€1000.00"/utf8>>, format_price(1000, 4, #{cur_symbol => "€", cur_sep => "€", cur_pos => left})),
	?_assertEqual(format_number(100, 2, 3), format_number(100, 2, 3, #{})),
	?_assertEqual(format_price(1000), format_price(1000, 2)),
	?_assertEqual(format_price(1000), format_price(1000, 2, <<>>)),
	?_assertEqual(format_price(1000), format_number(1000, 2, 2, #{}))
	].


format_bytes_test_() ->
	KB10_1000 = 10 * 1000,
	KB10_1024 = 10 * 1024,
	MB10_1000 = 10 * 1000 * 1000,
	MB10_1024 = 10 * 1024 * 1024,
	[
	?_assertEqual(<<"0KB">>, format_bytes(1023)),
	?_assertEqual(<<"1KB">>, format_bytes(1024)),
	?_assertEqual(<<"0KB">>, format_bytes(999, #{base => 10})),
	?_assertEqual(<<"1KB">>, format_bytes(1023, #{base => 10})),
	?_assertEqual(<<"9KB">>, format_bytes(KB10_1000)),
	?_assertEqual(<<"10KB">>, format_bytes(KB10_1024)),
	?_assertEqual(format_bytes(KB10_1000), format_bytes(KB10_1000, #{})),
	?_assertEqual(format_bytes(MB10_1000), format_bytes(MB10_1000, #{})),
	?_assertEqual(format_bytes(KB10_1024), format_bytes(KB10_1024, #{})),
	?_assertEqual(format_bytes(MB10_1024), format_bytes(MB10_1024, #{})),
	?_assertEqual(format_bytes(10000), format_bytes(10000, #{base => 2, units => auto})),
	?_assertEqual(format_bytes(10000), format_bytes(10000, #{base => 2, units => auto, to_type => bin})),
	?_assertEqual({9, 'KB'}, format_bytes(10000, #{to_type => int})),
	?_assertEqual(9, format_bytes(KB10_1000, #{to_type => int, units => 'KB'})),
	?_assertEqual(9, format_bytes(MB10_1000, #{to_type => int, units => 'MB'})),
	?_assertEqual({9, 'MB'}, format_bytes(MB10_1000, #{to_type => int, units => auto})),
	?_assertEqual(10, format_bytes(MB10_1024, #{to_type => int, units => 'MB'})),
	?_assertEqual({10, 'MB'}, format_bytes(MB10_1024, #{to_type => int})),
	?_assertEqual(<<"0MB">>, format_bytes(1000, #{units => 'MB'})),
	?_assertError({badarg, bad_int}, format_bytes(bad_int)),
	?_assertError({badarg, bad_opts}, format_bytes(1, bad_opts))
	].

-endif. % end of tests
