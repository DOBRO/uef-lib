%% Copyright (c) 2019-2021, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(uef_bin).

-export([binary_join/2, split/2, split/3]).
-export([repeat/2]).
-export([reverse/1, reverse_utf8/1]).
-export([replace/3, replace_chars/3]).
-export([random_latin_binary/2, random_binary_from_chars/2]).
-export([numeric_prefix/1]).
-export([strip_left/2]).

-type split_option() :: undefined | trim_all.

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% binary_join/2
-spec binary_join(ListOfBinaries :: [binary()], Separator :: binary()) -> binary().
%% @doc
%% Joins a list of binaries with separator into a single binary. Returns binary.
%% @end
binary_join([], _Sep) -> <<>>;
binary_join([Bin], _Sep) -> Bin;
binary_join([Head|Tail], Sep) ->
	lists:foldl(fun(Value, Acc) ->
		<<Acc/binary, Sep/binary, Value/binary>>
	end, Head, Tail).

%% split/2
-spec split(Binary :: binary(), Splitter :: binary()) -> ListOfBinaries :: [binary()].
%% @doc
%% Splits binary Binary with splitter Splitter into a list of binaries.
%% Works as binary:split/2 but is more performant in simple cases.
%% @end
split(B, Splitter) ->
	split(B, Splitter, undefined).


%% split/3
-spec split(Binary :: binary(), Splitter :: binary(), SplitOption:: split_option()) -> ListOfBinaries :: [binary()].
%% @doc
%% Splits binary Binary with splitter Splitter into a list of binaries.
%% Works as uef_bin:split/2 but removes all epmty `(<<>>)' chunks.
%% It can be used in simple cases instead of binary:split/3 for the reason that it's more performant.
%% @end
split(<<>>, _, _) -> [];
split(B, <<>>, _) -> [B];
split(B, Splitter, Option) ->
	SplitterBitSize = erlang:bit_size(Splitter),
	List = do_split(B, Splitter, SplitterBitSize, []),
	case Option of
		trim_all -> lists:filter(fun(<<>>) -> false; (_) -> true end, List);
		_ -> List
	end.

%% repeat/2
-spec repeat(Binary1 :: binary(), N :: pos_integer()) -> Binary2 :: binary().
%% @doc
%% Returns binary Binary2 consisting of Binary1 repeated N times.
%% @end
repeat(Bin, N) ->
	repeat(Bin, N, <<>>).


%% reverse/1
-spec reverse(binary()) -> binary().
%% @doc
%% Returns a binary in reverse byte order.
%% @end
reverse(B) ->
	S = erlang:bit_size(B),
	<<R:S/integer-little>> = B,
	<<R:S/integer-big>>.

%% reverse_utf8/1
-spec reverse_utf8(UTF8_Binary1 :: binary()) -> UTF8_Binary2 :: binary().
%% @doc
%% Returns a binary in reverse character order. Intended to work with UTF-8 binary strings.
%% @end
reverse_utf8(Bin) ->
	reverse_utf8(Bin, <<>>).

%% replace/3
-spec replace(Binary1 :: binary(), Chars :: binary(), OtherChars :: binary()) -> Binary2 :: binary().
%% @doc
%% Replaces chars Chars with other chars OtherChars in binary Binary1 and returns another binary Binary2.
%% Works as binary:replace/3 but more permormant and can be used in simple cases.
%% @end
replace(<<>>, _, _) -> <<>>;
replace(B, <<>>, _) -> B;
replace(B, C1, C2) ->
	C1BitSize = erlang:bit_size(C1),
	replace(B, C1, C1BitSize, C2, <<>>).


%% replace_chars/3
-spec replace_chars(Binary1 :: binary(), ListOfCharsToReplace :: [binary()], OtherChars :: binary()) -> Binary2 :: binary().
%% @doc
%% Replaces chars inluded in list ListOfCharsToReplace with other chars OtherChars in binary Binary1 and returns another binary Binary2.
%% @end
replace_chars(B0, [], _) -> B0;
replace_chars(B0, Chars, ToChar) ->
	lists:foldl(fun(Ch, B) ->
		replace(B, Ch, ToChar)
	end, B0, Chars).


%% random_latin_binary/2
-spec random_latin_binary(Length :: pos_integer(), CaseFlag :: lower | upper | any) -> binary().
%% @doc
%% Returns a random binary of size Length consisting of latins [a-zA-Z] and digits [0-9].
%% The second argument CaseFlag corresponds to a letter case, an atom 'lower', 'upper' or 'any'.
%% @end
random_latin_binary(Length, CaseFlag) ->
	Chars = case CaseFlag of
		lower -> <<"abcdefghijklmnopqrstuvwxyz0123456789">>;
		upper -> <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>;
		any -> <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>
	end,
	random_binary_from_chars(Length, Chars).


%% random_binary_from_chars/2
-spec random_binary_from_chars(Length :: pos_integer(), Chars :: binary()) -> binary().
%% @doc
%% Generates and returns a binary of size Length which consists of the given characters Chars.
%% @end
random_binary_from_chars(Length, Chars) ->
	Bsize = erlang:byte_size(Chars),
	lists:foldl(
		fun(_, Acc) ->
			RndChar = binary:at(Chars, rand:uniform(Bsize)-1),
			<< Acc/binary, RndChar >>
		end,
		<<>>,
		lists:seq(1, Length)
	).


%% numeric_prefix/1
-spec numeric_prefix(Binary :: binary()) -> DigitsOnlyOrEmptyBinary :: binary().
%% @doc
%% Returns new binary DigitsOnlyBinary which consists of digits [0-9] wich are at the beginning in the given binary Binary.
%% If Binary does not begin with digit, this function returns empty binary `(<<>>)'.
%% @end
numeric_prefix(B) -> numeric_prefix(B, <<>>).


%% strip_left/2
-spec strip_left(Bin :: binary(), Chars :: binary() | integer()) -> binary().
strip_left(Bin, Chars) when is_binary(Chars) ->
	do_strip_left(Bin, Chars, erlang:byte_size(Chars));
strip_left(Bin, Chars) when is_integer(Chars) ->
	strip_left(Bin, << Chars >>).

%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% repeat/3
-spec repeat(binary(), pos_integer(), binary()) -> binary().
repeat(_, N, Acc) when N < 1 ->
	Acc;
repeat(Bin, N, Acc) ->
	repeat(Bin, N-1, <<Acc/bits, Bin/bits>>).


%% replace/4
-spec replace(binary(), binary(), pos_integer(), binary(), binary()) -> binary().
replace(B, C1, C1BitSize, C2, Acc) ->
	case B of
		<<>> ->
			Acc;
		<<C1:C1BitSize/bits, Rest/bits>> ->
			replace(Rest, C1, C1BitSize, C2, <<Acc/bits, C2/bits>>); % replacement
		<<C, Rest/bits>> ->
			replace(Rest, C1, C1BitSize, C2, <<Acc/bits, C>>)
	end.


%% reverse_utf8/2
-spec reverse_utf8(binary(), binary()) -> binary().
reverse_utf8(<<>>, Acc) -> Acc;
reverse_utf8(<<U/utf8, Rest/bits>>, Acc) ->
 	reverse_utf8(Rest, <<U/utf8, Acc/bits>>).


%% do_split/3
-spec do_split(binary(), binary(), pos_integer(), [binary()]) -> [binary()].
do_split(B, Splitter, SplitterBitSize, List) ->
	case B of
		<<>> ->
			lists:reverse(List);
		<<Splitter:SplitterBitSize/bits, Rest/bits>> ->
			case List of
				[_|_] -> do_split(Rest, Splitter, SplitterBitSize, [<<>> | List]);
				[] -> do_split(Rest, Splitter, SplitterBitSize, [<<>>, <<>> | List])
			end;
		<<C, Rest/bits>> ->
			List2 = case List of
				[H|T] -> [<<H/bits, C>> | T];
				[] -> [<< C >>]
			end,
			do_split(Rest, Splitter, SplitterBitSize, List2)
	end.


%% numeric_prefix/2
-spec numeric_prefix(binary(), binary()) -> binary().
numeric_prefix(<< $0, Rest/bits >>, Acc) -> numeric_prefix(Rest, << Acc/bits, $0 >>);
numeric_prefix(<< $1, Rest/bits >>, Acc) -> numeric_prefix(Rest, << Acc/bits, $1 >>);
numeric_prefix(<< $2, Rest/bits >>, Acc) -> numeric_prefix(Rest, << Acc/bits, $2 >>);
numeric_prefix(<< $3, Rest/bits >>, Acc) -> numeric_prefix(Rest, << Acc/bits, $3 >>);
numeric_prefix(<< $4, Rest/bits >>, Acc) -> numeric_prefix(Rest, << Acc/bits, $4 >>);
numeric_prefix(<< $5, Rest/bits >>, Acc) -> numeric_prefix(Rest, << Acc/bits, $5 >>);
numeric_prefix(<< $6, Rest/bits >>, Acc) -> numeric_prefix(Rest, << Acc/bits, $6 >>);
numeric_prefix(<< $7, Rest/bits >>, Acc) -> numeric_prefix(Rest, << Acc/bits, $7 >>);
numeric_prefix(<< $8, Rest/bits >>, Acc) -> numeric_prefix(Rest, << Acc/bits, $8 >>);
numeric_prefix(<< $9, Rest/bits >>, Acc) -> numeric_prefix(Rest, << Acc/bits, $9 >>);
numeric_prefix(_, Acc) -> Acc.


%% do_strip_left/2
do_strip_left(<<>>, _, _) ->
	<<>>;
do_strip_left(Bin, Chars, CharsByteSize) ->
	case Bin of
		<< Chars:CharsByteSize/bytes, Rest/bits >> ->
			do_strip_left(Rest, Chars, CharsByteSize);
		_ -> Bin
	end.
