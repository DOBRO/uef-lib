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

-module(uef_bin_tests).

-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Test functions
%%%------------------------------------------------------------------------------

numeric_prefix_test_() ->
	[
	?_assertEqual(<<>>, uef_bin:numeric_prefix(<<"a234234">>)),
	?_assertEqual(<<"123">>, uef_bin:numeric_prefix(<<"123a456">>)),
	?_assertEqual(<<"123">>, uef_bin:numeric_prefix(<<"123">>)),
	?_assertEqual(<<"0">>, uef_bin:numeric_prefix(<<"0test">>)),
	?_assertEqual(<<"1">>, uef_bin:numeric_prefix(<<"1test">>)),
	?_assertEqual(<<"2">>, uef_bin:numeric_prefix(<<"2test">>)),
	?_assertEqual(<<"3">>, uef_bin:numeric_prefix(<<"3test">>)),
	?_assertEqual(<<"4">>, uef_bin:numeric_prefix(<<"4test">>)),
	?_assertEqual(<<"5">>, uef_bin:numeric_prefix(<<"5test">>)),
	?_assertEqual(<<"6">>, uef_bin:numeric_prefix(<<"6test">>)),
	?_assertEqual(<<"7">>, uef_bin:numeric_prefix(<<"7test">>)),
	?_assertEqual(<<"8">>, uef_bin:numeric_prefix(<<"8test">>)),
	?_assertEqual(<<"9">>, uef_bin:numeric_prefix(<<"9test">>))
	].

binary_join_test_() ->
	[
	?_assertEqual(<<>>, uef_bin:binary_join([], <<"any">>)),
	?_assertEqual(<<>>, uef_bin:binary_join([], <<>>)),
	?_assertEqual(<<"www.example.com">>, uef_bin:binary_join([<<"www">>, <<"example">>, <<"com">>], <<".">>)),
	?_assertEqual(<<"www">>, uef_bin:binary_join([<<"www">>], <<".">>))
	].

split_test_() ->
	[
	?_assertEqual([], uef_bin:split(<<>>, <<".">>)),
	?_assertEqual([], uef_bin:split(<<>>, <<>>)),
	?_assertEqual([<<".www.example.com.">>], uef_bin:split(<<".www.example.com.">>, <<>>, trim_all)),
	?_assertEqual([<<>>,<<"www">>,<<"example">>,<<"com">>,<<>>], uef_bin:split(<<".www.example.com.">>, <<".">>)),
	?_assertEqual([<<"www">>,<<"example">>,<<"com">>], uef_bin:split(<<"www.example.com">>, <<".">>)),
	?_assertEqual([<<"www.example.com">>], uef_bin:split(<<"www.example.com">>, <<"A">>)),
	?_assertEqual([<<"www">>,<<"example">>,<<"com">>], uef_bin:split(<<".....www.example.com....">>, <<".">>, trim_all))
	].

repeat_test_() ->
	[
	?_assertEqual(<<"0">>, uef_bin:repeat(<<"0">>, 1)),
	?_assertEqual(<<"aaaaa">>, uef_bin:repeat(<<"a">>, 5)),
	?_assertEqual(<<0>>, uef_bin:repeat(<<0>>, 1)),
	?_assertEqual(<<0,0,0>>, uef_bin:repeat(<<0>>, 3)),
	?_assertEqual(<<1,1,1,1>>, uef_bin:repeat(<<1,1>>, 2)),
	?_assertEqual(<<1,0,1,0>>, uef_bin:repeat(<<1,0>>, 2)),
	?_assertEqual(<<"abcabcabc">>, uef_bin:repeat(<<"abc">>, 3)),
	?_assertEqual(<<"ЖЖЖ"/utf8>>, uef_bin:repeat(<<"Ж"/utf8>>, 3))
	].

replace_test_() ->
	[
	?_assertEqual(<<>>, uef_bin:replace(<<>>, <<"aa">>, <<"bb">>)),
	?_assertEqual(<<"bbb">>, uef_bin:replace(<<"bbb">>, <<>>, <<"b">>)),
	?_assertEqual(<<"aZZdefgZZ">>, uef_bin:replace(<<"abcdefgbc">>, <<"bc">>, <<"ZZ">>)),
	?_assertEqual(<<"abcZZefgbc">>, uef_bin:replace(<<"abcdefgbc">>, <<"d">>, <<"ZZ">>))
	].

replace_chars_test_() ->
	[
	?_assertEqual(<<"bbb">>, uef_bin:replace_chars(<<"bbb">>, [], <<>>)),
	?_assertEqual(<<"wwwexamplecom">>, uef_bin:replace_chars(<<"..www.example.com.">>, [<<".">>], <<>>)),
	?_assertEqual(<<"examplecom">>, uef_bin:replace_chars(<<"..www.example.com.">>, [<<".">>, <<"w">>], <<>>))
	].

reverse_test_() ->
	[
	?_assertEqual(<<5,4,3,2,1>>, uef_bin:reverse(<<1,2,3,4,5>>)),
	?_assertEqual(<<"HGFEDCBA">>, uef_bin:reverse(<<"ABCDEFGH">>)),
	?_assertEqual(<<>>, uef_bin:reverse(<<>>)),
	?_assertEqual(<<0>>, uef_bin:reverse(<<0>>)),
	?_assertEqual(<<"0">>, uef_bin:reverse(<<"0">>)),
	?_assertEqual(<<1>>, uef_bin:reverse(<<1>>)),
	?_assertEqual(<<"1">>, uef_bin:reverse(<<"1">>)),
	?_assertEqual(<<0, 0, 0>>, uef_bin:reverse(<<0, 0, 0>>)),
	?_assertEqual(<<"ВБА">>, uef_bin:reverse(<<"АБВ">>))
	].

reverse_utf8_test_() ->
	[
	?_assertEqual(<<5,4,3,2,1,0>>, uef_bin:reverse_utf8(<<0,1,2,3,4,5>>)),
	?_assertEqual(<<"543210">>, uef_bin:reverse_utf8(<<"012345">>)),
	?_assertEqual(<<"HGFEDCBA">>, uef_bin:reverse_utf8(<<"ABCDEFGH">>)),
	?_assertEqual(<<>>, uef_bin:reverse_utf8(<<>>)),
	?_assertEqual(<<0>>, uef_bin:reverse_utf8(<<0>>)),
	?_assertEqual(<<"0">>, uef_bin:reverse_utf8(<<"0">>)),
	?_assertEqual(<<1>>, uef_bin:reverse_utf8(<<1>>)),
	?_assertEqual(<<"1">>, uef_bin:reverse_utf8(<<"1">>)),
	?_assertEqual(<<0, 0, 0>>, uef_bin:reverse_utf8(<<0, 0, 0>>)),
	?_assertEqual(<<"ВБА">>, uef_bin:reverse_utf8(<<"АБВ">>)),
	?_assertEqual(<<"ЖЁЕДГВБА"/utf8>>, uef_bin:reverse_utf8(<<"АБВГДЕЁЖ"/utf8>>)),
	?_assertEqual(<<7, 6, 5, 4, "ЖЁЕДГВБА"/utf8, 3, 2, 1>>, uef_bin:reverse_utf8(<<1, 2, 3, "АБВГДЕЁЖ"/utf8, 4, 5, 6, 7>>)),
	?_assertEqual(<<"eßartS eid"/utf8>>, uef_bin:reverse_utf8(<<"die Straße"/utf8>>)),
	?_assertEqual(<<"街條這"/utf8>>, uef_bin:reverse_utf8(<<"這條街"/utf8>>)),
	?_assertEqual(<<"好你"/utf8>>, uef_bin:reverse_utf8(<<"你好"/utf8>>)),
	?_assertEqual(<<"り通"/utf8>>, uef_bin:reverse_utf8(<<"通り"/utf8>>)),
	?_assertEqual(<<"はちにんこ"/utf8>>, uef_bin:reverse_utf8(<<"こんにちは"/utf8>>))
	].

random_latin_binary_test_() ->
	Length = 11,
	RandomLower = uef_bin:random_latin_binary(Length, lower),
	RandomUpper = uef_bin:random_latin_binary(Length, upper),
	RandomAny = uef_bin:random_latin_binary(Length, any),
	[
	?_assert(erlang:is_integer(Length) andalso Length > 0),
	?_assertEqual(Length, erlang:byte_size(RandomLower)),
	?_assertEqual(Length, erlang:byte_size(RandomUpper)),
	?_assertEqual(Length, erlang:byte_size(RandomAny)),
	?_assertEqual(ok, validate_random_latin_binary(RandomLower, lower)),
	?_assertEqual(ok, validate_random_latin_binary(RandomUpper, upper)),
	?_assertEqual(ok, validate_random_latin_binary(RandomAny, any))
	].


strip_left_test_() ->
	[
	?_assertEqual(<<>>, uef_bin:strip_left(<<>>, <<"any">>)),
	?_assertEqual(<<"test">>, uef_bin:strip_left(<<"test">>, <<>>)),
	?_assertEqual(<<"est">>, uef_bin:strip_left(<<"ttest">>, <<"t">>)),
	?_assertEqual(<<"est">>, uef_bin:strip_left(<<"ttest">>, <<"tt">>)),
	?_assertEqual(<<"test">>, uef_bin:strip_left(<<"tttest">>, <<"tt">>)),
	?_assertEqual(<<"est">>, uef_bin:strip_left(<<"ttest">>, $t)),
	?_assertEqual(<<"est">>, uef_bin:strip_left(<<"tttest">>, $t)),

	?_assertEqual(<<"st">>, uef_bin:strip_left(<<"test">>, <<"te">>)),
	?_assertEqual(<<"st">>, uef_bin:strip_left(<<"tetest">>, <<"te">>)),

	?_assertEqual(<<2,3,4,5>>, uef_bin:strip_left(<<1,1,1,2,3,4,5>>, <<1>>)),
	?_assertEqual(<<2,3,4,5>>, uef_bin:strip_left(<<1,1,1,2,3,4,5>>, 1)),

	?_assertEqual(<<"ривет"/utf8>>, uef_bin:strip_left(<<"привет"/utf8>>, <<"п"/utf8>>)),
	?_assertEqual(<<"ривет"/utf8>>, uef_bin:strip_left(<<"пппривет"/utf8>>, <<"п"/utf8>>)),
	?_assertEqual(<<"ивет"/utf8>>, uef_bin:strip_left(<<"привет"/utf8>>, <<"пр"/utf8>>))
	].


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------
validate_random_latin_binary(Bin, CaseFlag) ->
	case Bin of
		<<>> ->
			ok;
		<<C, Rest/bits>> ->
			IsInRange = case CaseFlag of
				lower ->
					(C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $z);
				upper ->
					(C >= $0 andalso C =< $9) orelse (C >= $A andalso C =< $Z);
				any   ->
					(C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z)
			end,
			case IsInRange of
				true  ->
					validate_random_latin_binary(Rest, CaseFlag);
				false ->
					{error, {invalid_char, C}}
			end;

		_ ->
			{error, other}
	end.
