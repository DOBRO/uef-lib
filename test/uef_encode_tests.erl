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

-module(uef_encode_tests).

-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

-ifdef(TEST).

html_encode_bin_test_() ->
	[
	?_assertEqual(<<"&lt;&gt;&amp;&copy;<br/>&trade;">>, uef_encode:html_encode_bin("<>&©\n™")),
	?_assertEqual(<<"&#9830;&plusmn;&Sigma;">>, uef_encode:html_encode_bin("♦±Σ"))
	].

html_encode_list_test_() ->
	[
	?_assertEqual([<<"&lt;">>,<<"&gt;">>,<<"&amp;">>,<<"&copy;">>,<<"<br/>">>,<<"&trade;">>], uef_encode:html_encode_list("<>&©\n™")),
	?_assertEqual([<<"&#9830;">>,<<"&plusmn;">>,<<"&Sigma;">>], uef_encode:html_encode_list("♦±Σ"))
	].

-endif.
