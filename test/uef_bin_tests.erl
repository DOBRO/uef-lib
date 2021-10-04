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
	?_assertEqual(<<"123">>, uef_bin:numeric_prefix(<<"123a456">>))
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
	?_assertEqual(<<5,4,3,2,1>>, uef_bin:reverse_utf8(<<1,2,3,4,5>>)),
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
