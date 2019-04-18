-module(uef_format).

-export([format_price/1, format_price/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% format_price/1
-spec format_price(Price:: number()) -> binary().
format_price(Price) -> format_price(Price, <<" ">>).

%% format_price/2
-spec format_price(Price :: number(), Delimiter :: binary() | string()) -> binary().
format_price(Price, Delimiter) when is_list(Delimiter) ->
	format_price(Price, erlang:list_to_binary(Delimiter));
format_price(Price, Delimiter) when is_integer(Price) ->
	format_price(erlang:float(Price), Delimiter);
format_price(Price, Delimiter) when is_float(Price) ->
	BinPrice = erlang:float_to_binary(Price, [{decimals, 2}]),
	case Price < 1000 of
		true -> BinPrice;
		false -> format_bin_price(BinPrice, Delimiter)
	end.

%% format_bin_price/2
-spec format_bin_price(binary(), binary()) -> binary().
format_bin_price(BinPrice, Delimiter) ->
	[Bi, Bf] = uef_bin:split(BinPrice, <<".">>),
	Nparts = byte_size(Bi) div 3,
	{Bi1, Bi2} = lists:foldl(
		fun(N, {B, Acc}) ->
			{B1, B2} = erlang:split_binary(B, byte_size(B) - 3),
			Acc2 = case N =:= 1 of
				true  -> B2;
				false -> << B2/binary, Delimiter/binary, Acc/binary >>
			end,
			{B1, Acc2}
		end,
		{Bi, <<>>},
		lists:seq(1, Nparts)
	),
	case byte_size(Bi1) > 0 of
		true  -> << Bi1/binary, Delimiter/binary, Bi2/binary, ".", Bf/binary >>;
		false -> << Bi2/binary, ".", Bf/binary >>
	end.


