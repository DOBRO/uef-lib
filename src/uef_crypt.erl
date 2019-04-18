-module(uef_crypt).

-export([md5_hex/1]).

%% md5_hex/1
-spec md5_hex(Data :: iodata()) -> binary().
md5_hex(Data) ->
	hstr(erlang:md5(Data)).


%% hstr/1
hstr(B) when is_binary(B) ->
	T = {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f},
	<< <<(element(X bsr 4 + 1, T)), (element(X band 16#0F + 1, T))>>
	|| <<X:8>> <= B >>.
