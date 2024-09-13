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

-module(uef_crypt_tests).

-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

md5_hex_test_() ->
    % Check with https://www.md5hashgenerator.com/
    [
    ?_assertEqual(<<"e2fc714c4727ee9395f324cd2e7f331f">>, uef_crypt:md5_hex(<<"abcd">>)),
    ?_assertEqual(<<"e2fc714c4727ee9395f324cd2e7f331f">>, uef_crypt:md5_hex("abcd")),
    ?_assertEqual(<<"e2fc714c4727ee9395f324cd2e7f331f">>, uef_crypt:md5_hex(["a", "bcd"])),
    ?_assertEqual(<<"e2fc714c4727ee9395f324cd2e7f331f">>, uef_crypt:md5_hex(["a", "b", ["c", ["d"]]])),
    ?_assertEqual(<<"6cd3556deb0da54bca060b4c39479839">>, uef_crypt:md5_hex(<<"Hello, world!">>)),
    ?_assertEqual(<<"aa3ad0839299f15e1e407d0d1a62f507">>, uef_crypt:md5_hex(<<"кто здесь?"/utf8>>)),
    ?_assertEqual(<<"56bf18da956bee82e008090e3d95ad7c">>, uef_crypt:md5_hex(<<"誰在這裡"/utf8>>))
    ].
