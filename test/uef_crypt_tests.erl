-module(uef_crypt_tests).
-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Generator
%%%------------------------------------------------------------------------------

uef_crypt_test_() ->
	[
		test_md5_hex()
	].


%%%------------------------------------------------------------------------------
%%%   Test functions
%%%------------------------------------------------------------------------------

%% Check with https://www.md5hashgenerator.com/
test_md5_hex() ->
	Fun = fun uef_crypt:md5_hex/1,
	[
		?_assertEqual(<<"e2fc714c4727ee9395f324cd2e7f331f">>, Fun(<<"abcd">>)),
		?_assertEqual(<<"6cd3556deb0da54bca060b4c39479839">>, Fun(<<"Hello, world!">>)),
		?_assertEqual(<<"aa3ad0839299f15e1e407d0d1a62f507">>, Fun(<<"кто здесь?"/utf8>>)),
		?_assertEqual(<<"56bf18da956bee82e008090e3d95ad7c">>, Fun(<<"誰在這裡"/utf8>>))
	].
