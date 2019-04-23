-module(uef_bin_tests).
-include_lib("eunit/include/eunit.hrl").


uef_bin_test_() ->
	[
		test_binary_join(),
		test_numeric_prefix(),
		test_split(),
		test_replace(),
		test_replace_chars()
	].

test_numeric_prefix() ->
	Fun = fun uef_bin:numeric_prefix/1,
	[
		?_assertEqual(<<>>, Fun(<<"a234234">>)),
		?_assertEqual(<<"123">>, Fun(<<"123a456">>))
	].

test_binary_join() ->
	Fun = fun uef_bin:binary_join/2,
	[
		?_assertEqual(<<"www.example.com">>, Fun([<<"www">>, <<"example">>, <<"com">>], <<".">>)),
		?_assertEqual(<<"www">>, Fun([<<"www">>], <<".">>))
	].

test_split() ->
	Fun2 = fun uef_bin:split/2,
	Fun3 = fun uef_bin:split/3,
	[
		?_assertEqual([<<>>,<<"www">>,<<"example">>,<<"com">>,<<>>], Fun2(<<".www.example.com.">>, <<".">>)),
		?_assertEqual([<<"www">>,<<"example">>,<<"com">>], Fun2(<<"www.example.com">>, <<".">>)),
		?_assertEqual([<<"www.example.com">>], Fun2(<<"www.example.com">>, <<"A">>)),
		?_assertEqual([<<"www">>,<<"example">>,<<"com">>], Fun3(<<".....www.example.com....">>, <<".">>, trim_all))
	].

test_replace() ->
	Fun = fun uef_bin:replace/3,
	[
		?_assertEqual(<<"aZZdefgZZ">>, Fun(<<"abcdefgbc">>, <<"bc">>, <<"ZZ">>)),
		?_assertEqual(<<"abcZZefgbc">>, Fun(<<"abcdefgbc">>, <<"d">>, <<"ZZ">>))
	].

test_replace_chars() ->
	Fun = fun uef_bin:replace_chars/3,
	[
		?_assertEqual(<<"wwwexamplecom">>, Fun(<<"..www.example.com.">>, [<<".">>], <<>>)),
		?_assertEqual(<<"examplecom">>, Fun(<<"..www.example.com.">>, [<<".">>, <<"w">>], <<>>))
	].
