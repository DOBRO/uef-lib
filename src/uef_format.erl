-module(uef_format).

-export([format_number/3, format_number/4]).
-export([format_price/1, format_price/2, format_price/3]).

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
-type number_format_erl_type() :: binary | string.
-type format_number_opts() :: #{
	thousands_sep => binary() | string(),
	decimal_point => binary() | string(),
	cur_symbol => binary() | string(),
	cur_pos => left | right,
	cur_sep => binary() | string(),
	erl_type => number_format_erl_type()
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
	RoundedNumber = uef_num:round_number(Number, Precision), % round to Precision before formatting
	format_number_1(RoundedNumber, Decimals, Opts).

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

%% format_number_1/3
-spec format_number_1(number(), decimals(), format_number_opts()) -> formatted_number().
format_number_1(Number, Decimals, Opts) ->
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
	% Format with remaining options
	RemainingOpts = maps:without([thousands_sep, decimal_point], Opts),
	format_number_2([currency, erl_type], FormattedNumber1, RemainingOpts).


%% format_number_2/3
-spec format_number_2(list(), binary(), map()) -> formatted_number().
format_number_2([], FmtNum, _) ->
	FmtNum;
format_number_2([currency|Tail], FmtNum, #{cur_symbol := CurSymbol0} = Opts) -> % currency
	CurSymbol = maybe_to_binary(CurSymbol0),
	CurSep = maybe_to_binary(maps:get(cur_sep, Opts, ?CURRENCY_SEP)),
	FmtNum2 = case maps:get(cur_pos, Opts, ?CURRENCY_POSITION) of
		left -> <<CurSymbol/binary, CurSep/binary, FmtNum/binary>>;
		_ -> <<FmtNum/binary, CurSep/binary, CurSymbol/binary>>
	end,
	format_number_2(Tail, FmtNum2, Opts);
format_number_2([erl_type|Tail], FmtNum, #{erl_type := string} = Opts)  -> % erl_type
	FmtNum2 = erlang:binary_to_list(FmtNum),
	format_number_2(Tail, FmtNum2, Opts);
format_number_2([_|Tail], FmtNum, Opts) -> % skip any other case
	format_number_2(Tail, FmtNum, Opts).


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
