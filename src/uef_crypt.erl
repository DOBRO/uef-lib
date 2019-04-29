-module(uef_crypt).

-export([md5_hex/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% md5_hex/1
-spec md5_hex(Data :: iodata()) -> binary().
md5_hex(Data) ->
	hstr(erlang:md5(Data)).


%% hstr/1
-spec hstr(binary()) -> binary().
hstr(B) when is_binary(B) ->
	T = {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f},
	<< <<(element(X bsr 4 + 1, T)), (element(X band 16#0F + 1, T))>>
	|| <<X:8>> <= B >>.


%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

-ifdef(TEST).

md5_hex_test_() ->
	% Check with https://www.md5hashgenerator.com/
	[
	?_assertEqual(<<"e2fc714c4727ee9395f324cd2e7f331f">>, md5_hex(<<"abcd">>)),
	?_assertEqual(<<"e2fc714c4727ee9395f324cd2e7f331f">>, md5_hex("abcd")),
	?_assertEqual(<<"e2fc714c4727ee9395f324cd2e7f331f">>, md5_hex(["a", "bcd"])),
	?_assertEqual(<<"e2fc714c4727ee9395f324cd2e7f331f">>, md5_hex(["a", "b", ["c", ["d"]]])),
	?_assertEqual(<<"6cd3556deb0da54bca060b4c39479839">>, md5_hex(<<"Hello, world!">>)),
	?_assertEqual(<<"aa3ad0839299f15e1e407d0d1a62f507">>, md5_hex(<<"кто здесь?"/utf8>>)),
	?_assertEqual(<<"56bf18da956bee82e008090e3d95ad7c">>, md5_hex(<<"誰在這裡"/utf8>>))
	].

-endif. % end of tests
