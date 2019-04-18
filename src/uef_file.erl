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
-spec read_file_info_fast(file_name()) -> {ok, file_info()} | {error, any()}.
read_file_info_fast(Filename) ->
	file:read_file_info(Filename, [raw, {time, posix}]).

%% read_file_fast/1
%% Should return the same as file:read_file/1
%% http://erlang.org/doc/man/file.html#read_file-1
-spec read_file_fast(file_name()) -> {ok, binary()} | {error, any()}.
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
