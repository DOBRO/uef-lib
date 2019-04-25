-module(uef_num).

-export([round_price/1, round_number/2]).

%% round_price/1
-spec round_price(number()) -> float().
round_price(Price) -> round_number(Price, 2).

%% round_number/2
-spec round_number(number(), integer()) -> float().
round_number(Number, Precision) ->
	P = math:pow(10, Precision),
	erlang:round(Number * P) / P.
