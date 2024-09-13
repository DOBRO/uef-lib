%% Copyright (c) 2019-2024, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(uef_format_tests).

-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

format_number_test_() ->
    [
    ?_assertEqual(<<"1.00">>, uef_format:format_number(1, 2, 2, #{})),
    ?_assertEqual(<<"1.99">>, uef_format:format_number(1.99, 2, 2, #{})),
    ?_assertEqual(<<"2.00">>, uef_format:format_number(1.99, 1, 2, #{})),
    ?_assertEqual(<<"10">>, uef_format:format_number(10, 2, 0, #{})),
    ?_assertEqual(<<"1 000.00">>, uef_format:format_number(1000, 0, 2, #{thousands_sep => " "})),
    ?_assertEqual(<<"1 000.00">>, uef_format:format_number(1000, 0, 2, #{thousands_sep => [32]})),
    ?_assertEqual(<<"1 000 999.00">>, uef_format:format_number(1000999, 2, 2, #{thousands_sep => <<" ">>})),
    ?_assertEqual(<<"2,000,000.00">>, uef_format:format_number(2000000, 2, 2, #{thousands_sep => <<",">>})),
    ?_assertEqual(<<"9 999 999 999.00">>, uef_format:format_number(9999999999, 2, 2, #{thousands_sep => <<" ">>})),
    ?_assertEqual(<<"99 999 999 999.99">>, uef_format:format_price(99999999999.99, 2, #{thousands_sep => <<" ">>})),
    ?_assertEqual(<<"999 999 999 999.99">>, uef_format:format_price(999999999999.99, 2, #{thousands_sep => <<" ">>})),
    ?_assertEqual(<<"999,999,999,999.99">>, uef_format:format_price(999999999999.99,  2, #{thousands_sep => <<",">>})),
    ?_assertEqual(<<"USD 1,234,567,890==4600">>, uef_format:format_number(1234567890.4567, 2, 4, #{thousands_sep => ",", decimal_point => "==", cur_symbol => "USD", cur_sep => " ", cur_pos => left})),
    ?_assertEqual(<<"$1000.88">>, uef_format:format_price(1000.8767, 4, "$")),
    ?_assertEqual(<<"1000.88 руб."/utf8>>, uef_format:format_price(1000.8767, 4, #{cur_symbol => <<"руб."/utf8>>, cur_sep => " ", cur_pos => right})),
    ?_assertEqual(<<"1000.88 руб."/utf8>>, uef_format:format_price(1000.8767, 4, #{cur_symbol => "руб.", cur_sep => " ", cur_pos => right})),
    ?_assertEqual(<<"€€1000.00"/utf8>>, uef_format:format_price(1000, 4, #{cur_symbol => "€", cur_sep => "€", cur_pos => left})),
    ?_assertEqual(uef_format:format_number(100, 2, 3), uef_format:format_number(100, 2, 3, #{})),
    ?_assertEqual(<<"199.500">>, uef_format:format_number(199.4567, 1, 3)),
    ?_assertEqual(<<"-199.500">>, uef_format:format_number(-199.4567, 1, 3)),
    ?_assertEqual(<<$1, 1, $0,$0,$0, $., $0,$0>>, uef_format:format_number(1000, 0, 2, #{thousands_sep => [1]})),
    ?_assertEqual(<<"1YB">>, uef_format:format_bytes(1 bsl 80, #{base => 2})),
    ?_assertEqual(uef_format:format_price(1000), uef_format:format_price(1000, 2)),
    ?_assertEqual(uef_format:format_price(1000), uef_format:format_price(1000, 2, <<>>)),
    ?_assertEqual(uef_format:format_price(1000), uef_format:format_number(1000, 2, 2, #{})),
    ?_assertError(badarg, uef_format:format_number(1000, 0, 2, #{thousands_sep => [-1]})),
    ?_assertError(badarg, uef_format:format_number(1000, 0, 2, #{thousands_sep => 32})),
    ?_assertError({invalid_units, 'BAD_UNITS'}, uef_format:format_bytes(100, #{units => 'BAD_UNITS'})),
    ?_assertError({invalid_output_type, list}, uef_format:format_bytes(100, #{to_type => list})),
    ?_assertError({invalid_separator, " "}, uef_format:format_bytes(100, #{sep => " "}))
    ].


format_bytes_test_() ->
    KB10_1000 = 10 * 1000,
    KB10_1024 = 10 * 1024,
    MB10_1000 = 10 * 1000 * 1000,
    MB10_1024 = 10 * 1024 * 1024,
    [
    ?_assertEqual(<<"0KB">>, uef_format:format_bytes(0)),
    ?_assertEqual(<<"0KB">>, uef_format:format_bytes(-0)),
    ?_assertEqual(<<"0KB">>, uef_format:format_bytes(1023)),
    ?_assertEqual(<<"1KB">>, uef_format:format_bytes(1024)),
    ?_assertEqual(<<"0KB">>, uef_format:format_bytes(999, #{base => 10})),
    ?_assertEqual(<<"1KB">>, uef_format:format_bytes(1023, #{base => 10})),
    ?_assertEqual(<<"-1KB">>, uef_format:format_bytes(-1023, #{base => 10})),
    ?_assertEqual(<<"9KB">>, uef_format:format_bytes(KB10_1000)),
    ?_assertEqual(<<"10KB">>, uef_format:format_bytes(KB10_1024)),
    ?_assertEqual(<<"10 KB">>, uef_format:format_bytes(KB10_1024, #{sep => <<" ">>})),
    ?_assertEqual(<<"10|KB">>, uef_format:format_bytes(KB10_1024, #{sep => <<"|">>})),
    ?_assertEqual(uef_format:format_bytes(KB10_1000), uef_format:format_bytes(KB10_1000, #{})),
    ?_assertEqual(uef_format:format_bytes(MB10_1000), uef_format:format_bytes(MB10_1000, #{})),
    ?_assertEqual(uef_format:format_bytes(KB10_1024), uef_format:format_bytes(KB10_1024, #{})),
    ?_assertEqual(uef_format:format_bytes(MB10_1024), uef_format:format_bytes(MB10_1024, #{})),
    ?_assertEqual(uef_format:format_bytes(10000), uef_format:format_bytes(10000, #{base => 2, units => auto})),
    ?_assertEqual(uef_format:format_bytes(10000), uef_format:format_bytes(10000, #{base => 2, units => auto, to_type => bin})),
    ?_assertEqual(uef_format:format_bytes(10000), uef_format:format_bytes(10000, #{base => 2, units => auto, to_type => bin, sep => <<>>})),
    ?_assertEqual({9, 'KB'}, uef_format:format_bytes(10000, #{to_type => int})),
    ?_assertEqual(9, uef_format:format_bytes(KB10_1000, #{to_type => int, units => 'KB'})),
    ?_assertEqual(9, uef_format:format_bytes(MB10_1000, #{to_type => int, units => 'MB'})),
    ?_assertEqual({9, 'MB'}, uef_format:format_bytes(MB10_1000, #{to_type => int, units => auto})),
    ?_assertEqual(10, uef_format:format_bytes(MB10_1024, #{to_type => int, units => 'MB'})),
    ?_assertEqual({10, 'MB'}, uef_format:format_bytes(MB10_1024, #{to_type => int})),
    ?_assertEqual(<<"0MB">>, uef_format:format_bytes(1000, #{units => 'MB'})),
    ?_assertError({badarg, bad_int}, uef_format:format_bytes(bad_int)),
    ?_assertError({badarg, bad_opts}, uef_format:format_bytes(1, bad_opts)),
    ?_assertError({invalid_base, 16}, uef_format:format_bytes(10000, #{base => 16}))
    ].


format_price_test_() ->
    [
    ?_assertError({badarg, bad_opts}, uef_format:format_price(1, 2, bad_opts))
    ].
