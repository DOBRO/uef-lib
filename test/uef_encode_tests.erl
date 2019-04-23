-module(uef_encode_tests).
-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Generator
%%%------------------------------------------------------------------------------

uef_encode_test_() ->
	[
		test_html_encode_bin(),
		test_html_encode_list()
	].


%%%------------------------------------------------------------------------------
%%%   Test functions
%%%------------------------------------------------------------------------------

test_html_encode_bin() ->
	Fun = fun uef_encode:html_encode_bin/1,
	[
		?_assertEqual(<<"&lt;&gt;&amp;&copy;<br/>&trade;">>, Fun("<>&©\n™")),
		?_assertEqual(<<"&#9830;&plusmn;&Sigma;">>, Fun("♦±Σ"))
	].

test_html_encode_list() ->
	Fun = fun uef_encode:html_encode_list/1,
	[
		?_assertEqual([<<"&lt;">>,<<"&gt;">>,<<"&amp;">>,<<"&copy;">>,<<"<br/>">>,<<"&trade;">>], Fun("<>&©\n™")),
		?_assertEqual([<<"&#9830;">>,<<"&plusmn;">>,<<"&Sigma;">>], Fun("♦±Σ"))
	].
