%% MIT License

%% Copyright (c) 2019, Sergei Semichev <chessvegas@chessvegas.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(uef_file).

-include_lib("kernel/include/file.hrl").

-export([read_file_fast/1, read_file_info_fast/1]).

%%%------------------------------------------------------------------------------
%%%   Types
%%%------------------------------------------------------------------------------

-type file_name() :: file:name_all().
-type file_info() :: file:file_info().

%%%------------------------------------------------------------------------------
%%%   API
%%%------------------------------------------------------------------------------

%% read_file_info_fast/1
%% Should return the same as file:read_file_info/1,2
%% http://erlang.org/doc/man/file.html#read_file_info-1
-spec read_file_info_fast(Filename :: file_name()) -> {ok, FileInfo :: file_info()} | {error, Reason :: any()}.
%% @doc
%% Retrieves information about local file.
%% Returns {ok, FileInfo} if successful, otherwise {error, Reason}.
%% Works as file:read_file_info/2 but optimized for local files. This is a wrapper of:
%% file:read_file_info(Filename, [raw, {time, posix}]).
%% @end
read_file_info_fast(Filename) ->
	file:read_file_info(Filename, [raw, {time, posix}]).

%% read_file_fast/1
%% Should return the same as file:read_file/1
%% http://erlang.org/doc/man/file.html#read_file-1
-spec read_file_fast(Filename :: file_name()) -> {ok, BinaryData :: binary()} | {error, Reason :: atom()}.
%% @doc
%% Reads contents of local file Filename
%% and returns {ok, BinaryData}, where BinaryData is a binary data object that contains the contents of Filename, or {error, Reason} if an error occurs.
%% This function is optimized for reading contents of local files, as no Erlang process is used.
%% It calls file:open/2 with options [read, raw, binary].
%% @end
read_file_fast(Filename) ->
	case read_file_info_fast(Filename) of
		{ok, #file_info{size = Filesize}} ->
			case open_and_read_bytes(Filename, Filesize) of
				{ok, _} = Result -> Result;
				eof -> {error, eof};
				{error, _} = Error -> Error
			end;
		{error, _} = Error ->
			Error
	end.


%%%------------------------------------------------------------------------------
%%%   Internal functions
%%%------------------------------------------------------------------------------

%% open_and_read_bytes/2
%% Should return the same as file:read/2
%% http://erlang.org/doc/man/file.html#read-2
-spec open_and_read_bytes(file_name(), integer()) -> {ok, binary()} | eof | {error, any()}.
open_and_read_bytes(Filename, Bytes) ->
	case file:open(Filename, [read, raw, binary]) of %% {ok, IoDevice} | {error, Reason}
		{ok, Fd} ->
			ReadResult = file:read(Fd, Bytes), %% {ok, Data} | eof | {error, Reason}
			_ = file:close(Fd),
			ReadResult;
		{error, _} = Error ->
			Error
	end.
