%% Copyright (c) 2019-2022, Sergei Semichev <chessvegas@chessvegas.com>. All Rights Reserved.
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

-module(uef_crypt).

-export([md5_hex/1]).

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% md5_hex/1
-spec md5_hex(IoData :: iodata()) -> Binary :: binary().
%% @doc
%% Returns binary Binary in hexadecimal form of md5 hash of the argument IoData
%% @end
md5_hex(IoData) ->
	hstr(erlang:md5(IoData)).


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% hstr/1
-spec hstr(binary()) -> binary().
hstr(B) when is_binary(B) ->
	T = {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f},
	<< <<(element(X bsr 4 + 1, T)), (element(X band 16#0F + 1, T))>>
	|| <<X:8>> <= B >>.
