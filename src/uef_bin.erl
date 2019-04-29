-module(uef_bin).

-export([binary_join/2, split/2, split/3]).
-export([replace/3, replace_chars/3]).
-export([random_latin_binary/2, random_binary_from_chars/2]).
-export([numeric_prefix/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-type split_option() :: undefined | trim_all.

%% binary_join/2
%% Examle:
%% binary_join([<<"Hello">>, <<"World">>], <<", ">>) -> <<"Hello, World">>
-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) -> <<>>;
binary_join([Bin], _Sep) -> Bin;
binary_join([Head|Tail], Sep) ->
	lists:foldl(fun(Value, Acc) ->
		<<Acc/binary, Sep/binary, Value/binary>>
	end, Head, Tail).

%% random_latin_binary/2
-spec random_latin_binary(Length :: pos_integer(), CaseFlag :: lower | upper | any) -> binary().
random_latin_binary(Length, CaseFlag) ->
	Chars = case CaseFlag of
		lower -> <<"abcdefghijklmnopqrstuvwxyz0123456789">>;
		upper -> <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>;
		any -> <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789">>
	end,
	random_binary_from_chars(Length, Chars).

%% random_binary_from_chars/2
-spec random_binary_from_chars(Length :: pos_integer(), Chars :: binary()) -> binary().
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

%% replace/3
%% Replaces chars with other chars in binary. Examples:
%% replace(<<"abcdefgbc">>, <<"bc">>, <<"ZZ">>) -> <<"aZZdefgZZ">>
%% replace(<<"abcdefgbc">>, <<$d>>, <<"ZZ">>) -> <<"abcZZefgbc">>
-spec replace(binary(), binary(), binary()) -> binary().
replace(<<>>, _, _) -> <<>>;
replace(B, <<>>, _) -> B;
replace(B, C1, C2) ->
	replace(B, {erlang:bit_size(C1), C1}, C2, <<>>).

%% replace/4
-spec replace(binary(), {pos_integer(), binary()}, binary(), binary()) -> binary().
replace(B, {BitSize, C1}, C2, Acc) ->
	case B of
		<<>> ->
			Acc;
		<<C1:BitSize/bits, Rest/bits>> ->
			replace(Rest, {BitSize, C1}, C2, <<Acc/bits, C2/bits>>); % replacement
		<<C, Rest/bits>> ->
			replace(Rest, {BitSize, C1}, C2, <<Acc/bits, C>>)
	end.

%% replace_chars/3
%% Replaces chars inluded in list with other chars. Examples:
%% replace_chars(<<"..www.google.com.">>, [<<".">>], <<>>) -> <<"wwwgooglecom">>
%% replace_chars(<<"..www.google.com.">>, [<<".">>, <<"w">>], <<>>) -> <<"googlecom">>
-spec replace_chars(binary(), [binary()], binary()) -> binary().
replace_chars(B0, [], _) -> B0;
replace_chars(B0, Chars, ToChar) ->
	lists:foldl(fun(Ch, B) ->
		replace(B, Ch, ToChar)
	end, B0, Chars).


%% split/2
%% Splits binary with delimiter and returns list of binary chunks
%% Examples:
%% split(<<".www.google.com.">>, <<".">>) -> [<<>>,<<"www">>,<<"google">>,<<"com">>,<<>>]
%% split(<<"www.google.com">>, <<".">>) -> [<<"www">>,<<"google">>,<<"com">>]
%% split(<<"www.google.com">>, <<"A">>) -> [<<"www.google.com">>]
-spec split(binary(), binary()) -> [binary()].
split(B, Splitter) ->
	split(B, Splitter, undefined).

%% split/3
%% The same as split/2 but with option.
%% With option 'trim_all' - removes all epmty chunks (<<>>).
%% Examples:
%% split(<<"..www.google.com.">>, <<".">>, trim_all) -> [<<"www">>,<<"google">>,<<"com">>]
-spec split(binary(), binary(), split_option()) -> [binary()].
split(<<>>, _, _) -> [];
split(B, <<>>, _) -> [B];
split(B, Splitter, Option) ->
	List = do_split(B, {erlang:bit_size(Splitter), Splitter}, []),
	case Option of
		trim_all -> lists:filter(fun(<<>>) -> false; (_) -> true end, List);
		_ -> List
	end.

%% do_split/3
-spec do_split(binary(), {pos_integer(), binary()}, [binary()]) -> [binary()].
do_split(B, {BitSize, Splitter}, List) ->
	case B of
		<<>> ->
			lists:reverse(List);
		<<Splitter:BitSize/bits, Rest/bits>> ->
			case List of
				[_|_] -> do_split(Rest, {BitSize, Splitter}, [<<>> | List]);
				[] -> do_split(Rest, {BitSize, Splitter}, [<<>>, <<>> | List])
			end;
		<<C, Rest/bits>> ->
			List2 = case List of
				[H|T] -> [<<H/bits, C>> | T];
				[] -> [<< C >>]
			end,
			do_split(Rest, {BitSize, Splitter}, List2)
	end.

%% numeric_prefix/1
%% Leaves only digits wich are at the beginning and removes the rest. Examles:
%% numeric_prefix(<<"3456sld1knskjd">>) -> <<"3456">>.
%% numeric_prefix(<<"ddd3456sld1knskjd">>) -> <<>>.
-spec numeric_prefix(binary()) -> binary().
numeric_prefix(B) -> numeric_prefix(B, <<>>).

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



%%%------------------------------------------------------------------------------
%%%   Test functions
%%%------------------------------------------------------------------------------

-ifdef(TEST).

numeric_prefix_test_() ->
	[
	?_assertEqual(<<>>, numeric_prefix(<<"a234234">>)),
	?_assertEqual(<<"123">>, numeric_prefix(<<"123a456">>))
	].

binary_join_test_() ->
	[
	?_assertEqual(<<"www.example.com">>, binary_join([<<"www">>, <<"example">>, <<"com">>], <<".">>)),
	?_assertEqual(<<"www">>, binary_join([<<"www">>], <<".">>))
	].

split_test_() ->
	[
	?_assertEqual([<<>>,<<"www">>,<<"example">>,<<"com">>,<<>>], split(<<".www.example.com.">>, <<".">>)),
	?_assertEqual([<<"www">>,<<"example">>,<<"com">>], split(<<"www.example.com">>, <<".">>)),
	?_assertEqual([<<"www.example.com">>], split(<<"www.example.com">>, <<"A">>)),
	?_assertEqual([<<"www">>,<<"example">>,<<"com">>], split(<<".....www.example.com....">>, <<".">>, trim_all))
	].

replace_test_() ->
	[
	?_assertEqual(<<"aZZdefgZZ">>, replace(<<"abcdefgbc">>, <<"bc">>, <<"ZZ">>)),
	?_assertEqual(<<"abcZZefgbc">>, replace(<<"abcdefgbc">>, <<"d">>, <<"ZZ">>))
	].

replace_chars_test_() ->
	[
	?_assertEqual(<<"wwwexamplecom">>, replace_chars(<<"..www.example.com.">>, [<<".">>], <<>>)),
	?_assertEqual(<<"examplecom">>, replace_chars(<<"..www.example.com.">>, [<<".">>, <<"w">>], <<>>))
	].

-endif. % end of tests
