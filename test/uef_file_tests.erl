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

-module(uef_file_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

read_file_info_fast_test_() ->
    TestDataDir = get_test_data_dir(),
    ExistingFile = filename:join([TestDataDir, "uef_file_1.txt"]),
    NonExistingFile = filename:join([TestDataDir, "uef_file_nonexisting.txt"]),
    [
    ?_assertMatch({ok, #file_info{type = regular}}, uef_file:read_file_info_fast(ExistingFile)),
    ?_assertEqual({error, enoent}, uef_file:read_file_info_fast(NonExistingFile))
    ].

read_file_fast_test_() ->
    TestDataDir = get_test_data_dir(),
    ExistingFile = filename:join([TestDataDir, "uef_file_1.txt"]),
    NonExistingFile = filename:join([TestDataDir, "uef_file_nonexisting.txt"]),
    ExpectedData = case os:type() of
        {win32, _}  -> <<"test1\r\n">>;
        _           -> <<"test1\n">>
    end,
    [
    ?_assertEqual({ok, ExpectedData}, uef_file:read_file_fast(ExistingFile)),
    ?_assertEqual({error, enoent}, uef_file:read_file_fast(NonExistingFile))
    ].


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

get_test_data_dir() ->
    LibDir = code:lib_dir(uef),
    filename:join([LibDir, "test", "data"]).
