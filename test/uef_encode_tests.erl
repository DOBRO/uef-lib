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

-module(uef_encode_tests).

-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------------
%%%   Tests
%%%------------------------------------------------------------------------------

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

win_to_utf8_test_() ->
	[
		?_assertEqual(<<16#0000/utf8>>, uef_encode:win_to_utf8(<<16#00>>)), %% NULL
		?_assertEqual(<<16#0001/utf8>>, uef_encode:win_to_utf8(<<16#01>>)), %% START OF HEADING
		?_assertEqual(<<16#0002/utf8>>, uef_encode:win_to_utf8(<<16#02>>)), %% START OF TEXT
		?_assertEqual(<<16#0003/utf8>>, uef_encode:win_to_utf8(<<16#03>>)), %% END OF TEXT
		?_assertEqual(<<16#0004/utf8>>, uef_encode:win_to_utf8(<<16#04>>)), %% END OF TRANSMISSION
		?_assertEqual(<<16#0005/utf8>>, uef_encode:win_to_utf8(<<16#05>>)), %% ENQUIRY
		?_assertEqual(<<16#0006/utf8>>, uef_encode:win_to_utf8(<<16#06>>)), %% ACKNOWLEDGE
		?_assertEqual(<<16#0007/utf8>>, uef_encode:win_to_utf8(<<16#07>>)), %% BELL
		?_assertEqual(<<16#0008/utf8>>, uef_encode:win_to_utf8(<<16#08>>)), %% BACKSPACE
		?_assertEqual(<<16#0009/utf8>>, uef_encode:win_to_utf8(<<16#09>>)), %% HORIZONTAL TABULATION
		?_assertEqual(<<16#000A/utf8>>, uef_encode:win_to_utf8(<<16#0A>>)), %% LINE FEED
		?_assertEqual(<<16#000B/utf8>>, uef_encode:win_to_utf8(<<16#0B>>)), %% VERTICAL TABULATION
		?_assertEqual(<<16#000C/utf8>>, uef_encode:win_to_utf8(<<16#0C>>)), %% FORM FEED
		?_assertEqual(<<16#000D/utf8>>, uef_encode:win_to_utf8(<<16#0D>>)), %% CARRIAGE RETURN
		?_assertEqual(<<16#000E/utf8>>, uef_encode:win_to_utf8(<<16#0E>>)), %% SHIFT OUT
		?_assertEqual(<<16#000F/utf8>>, uef_encode:win_to_utf8(<<16#0F>>)), %% SHIFT IN
		?_assertEqual(<<16#0010/utf8>>, uef_encode:win_to_utf8(<<16#10>>)), %% DATA LINK ESCAPE
		?_assertEqual(<<16#0011/utf8>>, uef_encode:win_to_utf8(<<16#11>>)), %% DEVICE CONTROL ONE
		?_assertEqual(<<16#0012/utf8>>, uef_encode:win_to_utf8(<<16#12>>)), %% DEVICE CONTROL TWO
		?_assertEqual(<<16#0013/utf8>>, uef_encode:win_to_utf8(<<16#13>>)), %% DEVICE CONTROL THREE
		?_assertEqual(<<16#0014/utf8>>, uef_encode:win_to_utf8(<<16#14>>)), %% DEVICE CONTROL FOUR
		?_assertEqual(<<16#0015/utf8>>, uef_encode:win_to_utf8(<<16#15>>)), %% NEGATIVE ACKNOWLEDGE
		?_assertEqual(<<16#0016/utf8>>, uef_encode:win_to_utf8(<<16#16>>)), %% SYNCHRONOUS IDLE
		?_assertEqual(<<16#0017/utf8>>, uef_encode:win_to_utf8(<<16#17>>)), %% END OF TRANSMISSION BLOCK
		?_assertEqual(<<16#0018/utf8>>, uef_encode:win_to_utf8(<<16#18>>)), %% CANCEL
		?_assertEqual(<<16#0019/utf8>>, uef_encode:win_to_utf8(<<16#19>>)), %% END OF MEDIUM
		?_assertEqual(<<16#001A/utf8>>, uef_encode:win_to_utf8(<<16#1A>>)), %% SUBSTITUTE
		?_assertEqual(<<16#001B/utf8>>, uef_encode:win_to_utf8(<<16#1B>>)), %% ESCAPE
		?_assertEqual(<<16#001C/utf8>>, uef_encode:win_to_utf8(<<16#1C>>)), %% FILE SEPARATOR
		?_assertEqual(<<16#001D/utf8>>, uef_encode:win_to_utf8(<<16#1D>>)), %% GROUP SEPARATOR
		?_assertEqual(<<16#001E/utf8>>, uef_encode:win_to_utf8(<<16#1E>>)), %% RECORD SEPARATOR
		?_assertEqual(<<16#001F/utf8>>, uef_encode:win_to_utf8(<<16#1F>>)), %% UNIT SEPARATOR
		?_assertEqual(<<16#0020/utf8>>, uef_encode:win_to_utf8(<<16#20>>)), %% SPACE
		?_assertEqual(<<16#0021/utf8>>, uef_encode:win_to_utf8(<<16#21>>)), %% EXCLAMATION MARK
		?_assertEqual(<<16#0022/utf8>>, uef_encode:win_to_utf8(<<16#22>>)), %% QUOTATION MARK
		?_assertEqual(<<16#0023/utf8>>, uef_encode:win_to_utf8(<<16#23>>)), %% NUMBER SIGN
		?_assertEqual(<<16#0024/utf8>>, uef_encode:win_to_utf8(<<16#24>>)), %% DOLLAR SIGN
		?_assertEqual(<<16#0025/utf8>>, uef_encode:win_to_utf8(<<16#25>>)), %% PERCENT SIGN
		?_assertEqual(<<16#0026/utf8>>, uef_encode:win_to_utf8(<<16#26>>)), %% AMPERSAND
		?_assertEqual(<<16#0027/utf8>>, uef_encode:win_to_utf8(<<16#27>>)), %% APOSTROPHE
		?_assertEqual(<<16#0028/utf8>>, uef_encode:win_to_utf8(<<16#28>>)), %% LEFT PARENTHESIS
		?_assertEqual(<<16#0029/utf8>>, uef_encode:win_to_utf8(<<16#29>>)), %% RIGHT PARENTHESIS
		?_assertEqual(<<16#002A/utf8>>, uef_encode:win_to_utf8(<<16#2A>>)), %% ASTERISK
		?_assertEqual(<<16#002B/utf8>>, uef_encode:win_to_utf8(<<16#2B>>)), %% PLUS SIGN
		?_assertEqual(<<16#002C/utf8>>, uef_encode:win_to_utf8(<<16#2C>>)), %% COMMA
		?_assertEqual(<<16#002D/utf8>>, uef_encode:win_to_utf8(<<16#2D>>)), %% HYPHEN-MINUS
		?_assertEqual(<<16#002E/utf8>>, uef_encode:win_to_utf8(<<16#2E>>)), %% FULL STOP
		?_assertEqual(<<16#002F/utf8>>, uef_encode:win_to_utf8(<<16#2F>>)), %% SOLIDUS
		?_assertEqual(<<16#0030/utf8>>, uef_encode:win_to_utf8(<<16#30>>)), %% DIGIT ZERO
		?_assertEqual(<<16#0031/utf8>>, uef_encode:win_to_utf8(<<16#31>>)), %% DIGIT ONE
		?_assertEqual(<<16#0032/utf8>>, uef_encode:win_to_utf8(<<16#32>>)), %% DIGIT TWO
		?_assertEqual(<<16#0033/utf8>>, uef_encode:win_to_utf8(<<16#33>>)), %% DIGIT THREE
		?_assertEqual(<<16#0034/utf8>>, uef_encode:win_to_utf8(<<16#34>>)), %% DIGIT FOUR
		?_assertEqual(<<16#0035/utf8>>, uef_encode:win_to_utf8(<<16#35>>)), %% DIGIT FIVE
		?_assertEqual(<<16#0036/utf8>>, uef_encode:win_to_utf8(<<16#36>>)), %% DIGIT SIX
		?_assertEqual(<<16#0037/utf8>>, uef_encode:win_to_utf8(<<16#37>>)), %% DIGIT SEVEN
		?_assertEqual(<<16#0038/utf8>>, uef_encode:win_to_utf8(<<16#38>>)), %% DIGIT EIGHT
		?_assertEqual(<<16#0039/utf8>>, uef_encode:win_to_utf8(<<16#39>>)), %% DIGIT NINE
		?_assertEqual(<<16#003A/utf8>>, uef_encode:win_to_utf8(<<16#3A>>)), %% COLON
		?_assertEqual(<<16#003B/utf8>>, uef_encode:win_to_utf8(<<16#3B>>)), %% SEMICOLON
		?_assertEqual(<<16#003C/utf8>>, uef_encode:win_to_utf8(<<16#3C>>)), %% LESS-THAN SIGN
		?_assertEqual(<<16#003D/utf8>>, uef_encode:win_to_utf8(<<16#3D>>)), %% EQUALS SIGN
		?_assertEqual(<<16#003E/utf8>>, uef_encode:win_to_utf8(<<16#3E>>)), %% GREATER-THAN SIGN
		?_assertEqual(<<16#003F/utf8>>, uef_encode:win_to_utf8(<<16#3F>>)), %% QUESTION MARK
		?_assertEqual(<<16#0040/utf8>>, uef_encode:win_to_utf8(<<16#40>>)), %% COMMERCIAL AT
		?_assertEqual(<<16#0041/utf8>>, uef_encode:win_to_utf8(<<16#41>>)), %% LATIN CAPITAL LETTER A
		?_assertEqual(<<16#0042/utf8>>, uef_encode:win_to_utf8(<<16#42>>)), %% LATIN CAPITAL LETTER B
		?_assertEqual(<<16#0043/utf8>>, uef_encode:win_to_utf8(<<16#43>>)), %% LATIN CAPITAL LETTER C
		?_assertEqual(<<16#0044/utf8>>, uef_encode:win_to_utf8(<<16#44>>)), %% LATIN CAPITAL LETTER D
		?_assertEqual(<<16#0045/utf8>>, uef_encode:win_to_utf8(<<16#45>>)), %% LATIN CAPITAL LETTER E
		?_assertEqual(<<16#0046/utf8>>, uef_encode:win_to_utf8(<<16#46>>)), %% LATIN CAPITAL LETTER F
		?_assertEqual(<<16#0047/utf8>>, uef_encode:win_to_utf8(<<16#47>>)), %% LATIN CAPITAL LETTER G
		?_assertEqual(<<16#0048/utf8>>, uef_encode:win_to_utf8(<<16#48>>)), %% LATIN CAPITAL LETTER H
		?_assertEqual(<<16#0049/utf8>>, uef_encode:win_to_utf8(<<16#49>>)), %% LATIN CAPITAL LETTER I
		?_assertEqual(<<16#004A/utf8>>, uef_encode:win_to_utf8(<<16#4A>>)), %% LATIN CAPITAL LETTER J
		?_assertEqual(<<16#004B/utf8>>, uef_encode:win_to_utf8(<<16#4B>>)), %% LATIN CAPITAL LETTER K
		?_assertEqual(<<16#004C/utf8>>, uef_encode:win_to_utf8(<<16#4C>>)), %% LATIN CAPITAL LETTER L
		?_assertEqual(<<16#004D/utf8>>, uef_encode:win_to_utf8(<<16#4D>>)), %% LATIN CAPITAL LETTER M
		?_assertEqual(<<16#004E/utf8>>, uef_encode:win_to_utf8(<<16#4E>>)), %% LATIN CAPITAL LETTER N
		?_assertEqual(<<16#004F/utf8>>, uef_encode:win_to_utf8(<<16#4F>>)), %% LATIN CAPITAL LETTER O
		?_assertEqual(<<16#0050/utf8>>, uef_encode:win_to_utf8(<<16#50>>)), %% LATIN CAPITAL LETTER P
		?_assertEqual(<<16#0051/utf8>>, uef_encode:win_to_utf8(<<16#51>>)), %% LATIN CAPITAL LETTER Q
		?_assertEqual(<<16#0052/utf8>>, uef_encode:win_to_utf8(<<16#52>>)), %% LATIN CAPITAL LETTER R
		?_assertEqual(<<16#0053/utf8>>, uef_encode:win_to_utf8(<<16#53>>)), %% LATIN CAPITAL LETTER S
		?_assertEqual(<<16#0054/utf8>>, uef_encode:win_to_utf8(<<16#54>>)), %% LATIN CAPITAL LETTER T
		?_assertEqual(<<16#0055/utf8>>, uef_encode:win_to_utf8(<<16#55>>)), %% LATIN CAPITAL LETTER U
		?_assertEqual(<<16#0056/utf8>>, uef_encode:win_to_utf8(<<16#56>>)), %% LATIN CAPITAL LETTER V
		?_assertEqual(<<16#0057/utf8>>, uef_encode:win_to_utf8(<<16#57>>)), %% LATIN CAPITAL LETTER W
		?_assertEqual(<<16#0058/utf8>>, uef_encode:win_to_utf8(<<16#58>>)), %% LATIN CAPITAL LETTER X
		?_assertEqual(<<16#0059/utf8>>, uef_encode:win_to_utf8(<<16#59>>)), %% LATIN CAPITAL LETTER Y
		?_assertEqual(<<16#005A/utf8>>, uef_encode:win_to_utf8(<<16#5A>>)), %% LATIN CAPITAL LETTER Z
		?_assertEqual(<<16#005B/utf8>>, uef_encode:win_to_utf8(<<16#5B>>)), %% LEFT SQUARE BRACKET
		?_assertEqual(<<16#005C/utf8>>, uef_encode:win_to_utf8(<<16#5C>>)), %% REVERSE SOLIDUS
		?_assertEqual(<<16#005D/utf8>>, uef_encode:win_to_utf8(<<16#5D>>)), %% RIGHT SQUARE BRACKET
		?_assertEqual(<<16#005E/utf8>>, uef_encode:win_to_utf8(<<16#5E>>)), %% CIRCUMFLEX ACCENT
		?_assertEqual(<<16#005F/utf8>>, uef_encode:win_to_utf8(<<16#5F>>)), %% LOW LINE
		?_assertEqual(<<16#0060/utf8>>, uef_encode:win_to_utf8(<<16#60>>)), %% GRAVE ACCENT
		?_assertEqual(<<16#0061/utf8>>, uef_encode:win_to_utf8(<<16#61>>)), %% LATIN SMALL LETTER A
		?_assertEqual(<<16#0062/utf8>>, uef_encode:win_to_utf8(<<16#62>>)), %% LATIN SMALL LETTER B
		?_assertEqual(<<16#0063/utf8>>, uef_encode:win_to_utf8(<<16#63>>)), %% LATIN SMALL LETTER C
		?_assertEqual(<<16#0064/utf8>>, uef_encode:win_to_utf8(<<16#64>>)), %% LATIN SMALL LETTER D
		?_assertEqual(<<16#0065/utf8>>, uef_encode:win_to_utf8(<<16#65>>)), %% LATIN SMALL LETTER E
		?_assertEqual(<<16#0066/utf8>>, uef_encode:win_to_utf8(<<16#66>>)), %% LATIN SMALL LETTER F
		?_assertEqual(<<16#0067/utf8>>, uef_encode:win_to_utf8(<<16#67>>)), %% LATIN SMALL LETTER G
		?_assertEqual(<<16#0068/utf8>>, uef_encode:win_to_utf8(<<16#68>>)), %% LATIN SMALL LETTER H
		?_assertEqual(<<16#0069/utf8>>, uef_encode:win_to_utf8(<<16#69>>)), %% LATIN SMALL LETTER I
		?_assertEqual(<<16#006A/utf8>>, uef_encode:win_to_utf8(<<16#6A>>)), %% LATIN SMALL LETTER J
		?_assertEqual(<<16#006B/utf8>>, uef_encode:win_to_utf8(<<16#6B>>)), %% LATIN SMALL LETTER K
		?_assertEqual(<<16#006C/utf8>>, uef_encode:win_to_utf8(<<16#6C>>)), %% LATIN SMALL LETTER L
		?_assertEqual(<<16#006D/utf8>>, uef_encode:win_to_utf8(<<16#6D>>)), %% LATIN SMALL LETTER M
		?_assertEqual(<<16#006E/utf8>>, uef_encode:win_to_utf8(<<16#6E>>)), %% LATIN SMALL LETTER N
		?_assertEqual(<<16#006F/utf8>>, uef_encode:win_to_utf8(<<16#6F>>)), %% LATIN SMALL LETTER O
		?_assertEqual(<<16#0070/utf8>>, uef_encode:win_to_utf8(<<16#70>>)), %% LATIN SMALL LETTER P
		?_assertEqual(<<16#0071/utf8>>, uef_encode:win_to_utf8(<<16#71>>)), %% LATIN SMALL LETTER Q
		?_assertEqual(<<16#0072/utf8>>, uef_encode:win_to_utf8(<<16#72>>)), %% LATIN SMALL LETTER R
		?_assertEqual(<<16#0073/utf8>>, uef_encode:win_to_utf8(<<16#73>>)), %% LATIN SMALL LETTER S
		?_assertEqual(<<16#0074/utf8>>, uef_encode:win_to_utf8(<<16#74>>)), %% LATIN SMALL LETTER T
		?_assertEqual(<<16#0075/utf8>>, uef_encode:win_to_utf8(<<16#75>>)), %% LATIN SMALL LETTER U
		?_assertEqual(<<16#0076/utf8>>, uef_encode:win_to_utf8(<<16#76>>)), %% LATIN SMALL LETTER V
		?_assertEqual(<<16#0077/utf8>>, uef_encode:win_to_utf8(<<16#77>>)), %% LATIN SMALL LETTER W
		?_assertEqual(<<16#0078/utf8>>, uef_encode:win_to_utf8(<<16#78>>)), %% LATIN SMALL LETTER X
		?_assertEqual(<<16#0079/utf8>>, uef_encode:win_to_utf8(<<16#79>>)), %% LATIN SMALL LETTER Y
		?_assertEqual(<<16#007A/utf8>>, uef_encode:win_to_utf8(<<16#7A>>)), %% LATIN SMALL LETTER Z
		?_assertEqual(<<16#007B/utf8>>, uef_encode:win_to_utf8(<<16#7B>>)), %% LEFT CURLY BRACKET
		?_assertEqual(<<16#007C/utf8>>, uef_encode:win_to_utf8(<<16#7C>>)), %% VERTICAL LINE
		?_assertEqual(<<16#007D/utf8>>, uef_encode:win_to_utf8(<<16#7D>>)), %% RIGHT CURLY BRACKET
		?_assertEqual(<<16#007E/utf8>>, uef_encode:win_to_utf8(<<16#7E>>)), %% TILDE
		?_assertEqual(<<16#007F/utf8>>, uef_encode:win_to_utf8(<<16#7F>>)), %% DELETE
		?_assertEqual(<<16#0402/utf8>>, uef_encode:win_to_utf8(<<16#80>>)), %% CYRILLIC CAPITAL LETTER DJE
		?_assertEqual(<<16#0403/utf8>>, uef_encode:win_to_utf8(<<16#81>>)), %% CYRILLIC CAPITAL LETTER GJE
		?_assertEqual(<<16#201A/utf8>>, uef_encode:win_to_utf8(<<16#82>>)), %% SINGLE LOW-9 QUOTATION MARK
		?_assertEqual(<<16#0453/utf8>>, uef_encode:win_to_utf8(<<16#83>>)), %% CYRILLIC SMALL LETTER GJE
		?_assertEqual(<<16#201E/utf8>>, uef_encode:win_to_utf8(<<16#84>>)), %% DOUBLE LOW-9 QUOTATION MARK
		?_assertEqual(<<16#2026/utf8>>, uef_encode:win_to_utf8(<<16#85>>)), %% HORIZONTAL ELLIPSIS
		?_assertEqual(<<16#2020/utf8>>, uef_encode:win_to_utf8(<<16#86>>)), %% DAGGER
		?_assertEqual(<<16#2021/utf8>>, uef_encode:win_to_utf8(<<16#87>>)), %% DOUBLE DAGGER
		?_assertEqual(<<16#20AC/utf8>>, uef_encode:win_to_utf8(<<16#88>>)), %% EURO SIGN
		?_assertEqual(<<16#2030/utf8>>, uef_encode:win_to_utf8(<<16#89>>)), %% PER MILLE SIGN
		?_assertEqual(<<16#0409/utf8>>, uef_encode:win_to_utf8(<<16#8A>>)), %% CYRILLIC CAPITAL LETTER LJE
		?_assertEqual(<<16#2039/utf8>>, uef_encode:win_to_utf8(<<16#8B>>)), %% SINGLE LEFT-POINTING ANGLE QUOTATION MARK
		?_assertEqual(<<16#040A/utf8>>, uef_encode:win_to_utf8(<<16#8C>>)), %% CYRILLIC CAPITAL LETTER NJE
		?_assertEqual(<<16#040C/utf8>>, uef_encode:win_to_utf8(<<16#8D>>)), %% CYRILLIC CAPITAL LETTER KJE
		?_assertEqual(<<16#040B/utf8>>, uef_encode:win_to_utf8(<<16#8E>>)), %% CYRILLIC CAPITAL LETTER TSHE
		?_assertEqual(<<16#040F/utf8>>, uef_encode:win_to_utf8(<<16#8F>>)), %% CYRILLIC CAPITAL LETTER DZHE
		?_assertEqual(<<16#0452/utf8>>, uef_encode:win_to_utf8(<<16#90>>)), %% CYRILLIC SMALL LETTER DJE
		?_assertEqual(<<16#2018/utf8>>, uef_encode:win_to_utf8(<<16#91>>)), %% LEFT SINGLE QUOTATION MARK
		?_assertEqual(<<16#2019/utf8>>, uef_encode:win_to_utf8(<<16#92>>)), %% RIGHT SINGLE QUOTATION MARK
		?_assertEqual(<<16#201C/utf8>>, uef_encode:win_to_utf8(<<16#93>>)), %% LEFT DOUBLE QUOTATION MARK
		?_assertEqual(<<16#201D/utf8>>, uef_encode:win_to_utf8(<<16#94>>)), %% RIGHT DOUBLE QUOTATION MARK
		?_assertEqual(<<16#2022/utf8>>, uef_encode:win_to_utf8(<<16#95>>)), %% BULLET
		?_assertEqual(<<16#2013/utf8>>, uef_encode:win_to_utf8(<<16#96>>)), %% EN DASH
		?_assertEqual(<<16#2014/utf8>>, uef_encode:win_to_utf8(<<16#97>>)), %% EM DASH
		?_assertEqual(<<16#2122/utf8>>, uef_encode:win_to_utf8(<<16#99>>)), %% TRADE MARK SIGN
		?_assertEqual(<<16#0459/utf8>>, uef_encode:win_to_utf8(<<16#9A>>)), %% CYRILLIC SMALL LETTER LJE
		?_assertEqual(<<16#203A/utf8>>, uef_encode:win_to_utf8(<<16#9B>>)), %% SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
		?_assertEqual(<<16#045A/utf8>>, uef_encode:win_to_utf8(<<16#9C>>)), %% CYRILLIC SMALL LETTER NJE
		?_assertEqual(<<16#045C/utf8>>, uef_encode:win_to_utf8(<<16#9D>>)), %% CYRILLIC SMALL LETTER KJE
		?_assertEqual(<<16#045B/utf8>>, uef_encode:win_to_utf8(<<16#9E>>)), %% CYRILLIC SMALL LETTER TSHE
		?_assertEqual(<<16#045F/utf8>>, uef_encode:win_to_utf8(<<16#9F>>)), %% CYRILLIC SMALL LETTER DZHE
		?_assertEqual(<<16#00A0/utf8>>, uef_encode:win_to_utf8(<<16#A0>>)), %% NO-BREAK SPACE
		?_assertEqual(<<16#040E/utf8>>, uef_encode:win_to_utf8(<<16#A1>>)), %% CYRILLIC CAPITAL LETTER SHORT U
		?_assertEqual(<<16#045E/utf8>>, uef_encode:win_to_utf8(<<16#A2>>)), %% CYRILLIC SMALL LETTER SHORT U
		?_assertEqual(<<16#0408/utf8>>, uef_encode:win_to_utf8(<<16#A3>>)), %% CYRILLIC CAPITAL LETTER JE
		?_assertEqual(<<16#00A4/utf8>>, uef_encode:win_to_utf8(<<16#A4>>)), %% CURRENCY SIGN
		?_assertEqual(<<16#0490/utf8>>, uef_encode:win_to_utf8(<<16#A5>>)), %% CYRILLIC CAPITAL LETTER GHE WITH UPTURN
		?_assertEqual(<<16#00A6/utf8>>, uef_encode:win_to_utf8(<<16#A6>>)), %% BROKEN BAR
		?_assertEqual(<<16#00A7/utf8>>, uef_encode:win_to_utf8(<<16#A7>>)), %% SECTION SIGN
		?_assertEqual(<<16#0401/utf8>>, uef_encode:win_to_utf8(<<16#A8>>)), %% CYRILLIC CAPITAL LETTER IO
		?_assertEqual(<<16#00A9/utf8>>, uef_encode:win_to_utf8(<<16#A9>>)), %% COPYRIGHT SIGN
		?_assertEqual(<<16#0404/utf8>>, uef_encode:win_to_utf8(<<16#AA>>)), %% CYRILLIC CAPITAL LETTER UKRAINIAN IE
		?_assertEqual(<<16#00AB/utf8>>, uef_encode:win_to_utf8(<<16#AB>>)), %% LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
		?_assertEqual(<<16#00AC/utf8>>, uef_encode:win_to_utf8(<<16#AC>>)), %% NOT SIGN
		?_assertEqual(<<16#00AD/utf8>>, uef_encode:win_to_utf8(<<16#AD>>)), %% SOFT HYPHEN
		?_assertEqual(<<16#00AE/utf8>>, uef_encode:win_to_utf8(<<16#AE>>)), %% REGISTERED SIGN
		?_assertEqual(<<16#0407/utf8>>, uef_encode:win_to_utf8(<<16#AF>>)), %% CYRILLIC CAPITAL LETTER YI
		?_assertEqual(<<16#00B0/utf8>>, uef_encode:win_to_utf8(<<16#B0>>)), %% DEGREE SIGN
		?_assertEqual(<<16#00B1/utf8>>, uef_encode:win_to_utf8(<<16#B1>>)), %% PLUS-MINUS SIGN
		?_assertEqual(<<16#0406/utf8>>, uef_encode:win_to_utf8(<<16#B2>>)), %% CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
		?_assertEqual(<<16#0456/utf8>>, uef_encode:win_to_utf8(<<16#B3>>)), %% CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
		?_assertEqual(<<16#0491/utf8>>, uef_encode:win_to_utf8(<<16#B4>>)), %% CYRILLIC SMALL LETTER GHE WITH UPTURN
		?_assertEqual(<<16#00B5/utf8>>, uef_encode:win_to_utf8(<<16#B5>>)), %% MICRO SIGN
		?_assertEqual(<<16#00B6/utf8>>, uef_encode:win_to_utf8(<<16#B6>>)), %% PILCROW SIGN
		?_assertEqual(<<16#00B7/utf8>>, uef_encode:win_to_utf8(<<16#B7>>)), %% MIDDLE DOT
		?_assertEqual(<<16#0451/utf8>>, uef_encode:win_to_utf8(<<16#B8>>)), %% CYRILLIC SMALL LETTER IO
		?_assertEqual(<<16#2116/utf8>>, uef_encode:win_to_utf8(<<16#B9>>)), %% NUMERO SIGN
		?_assertEqual(<<16#0454/utf8>>, uef_encode:win_to_utf8(<<16#BA>>)), %% CYRILLIC SMALL LETTER UKRAINIAN IE
		?_assertEqual(<<16#00BB/utf8>>, uef_encode:win_to_utf8(<<16#BB>>)), %% RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
		?_assertEqual(<<16#0458/utf8>>, uef_encode:win_to_utf8(<<16#BC>>)), %% CYRILLIC SMALL LETTER JE
		?_assertEqual(<<16#0405/utf8>>, uef_encode:win_to_utf8(<<16#BD>>)), %% CYRILLIC CAPITAL LETTER DZE
		?_assertEqual(<<16#0455/utf8>>, uef_encode:win_to_utf8(<<16#BE>>)), %% CYRILLIC SMALL LETTER DZE
		?_assertEqual(<<16#0457/utf8>>, uef_encode:win_to_utf8(<<16#BF>>)), %% CYRILLIC SMALL LETTER YI
		?_assertEqual(<<16#0410/utf8>>, uef_encode:win_to_utf8(<<16#C0>>)), %% CYRILLIC CAPITAL LETTER A
		?_assertEqual(<<16#0411/utf8>>, uef_encode:win_to_utf8(<<16#C1>>)), %% CYRILLIC CAPITAL LETTER BE
		?_assertEqual(<<16#0412/utf8>>, uef_encode:win_to_utf8(<<16#C2>>)), %% CYRILLIC CAPITAL LETTER VE
		?_assertEqual(<<16#0413/utf8>>, uef_encode:win_to_utf8(<<16#C3>>)), %% CYRILLIC CAPITAL LETTER GHE
		?_assertEqual(<<16#0414/utf8>>, uef_encode:win_to_utf8(<<16#C4>>)), %% CYRILLIC CAPITAL LETTER DE
		?_assertEqual(<<16#0415/utf8>>, uef_encode:win_to_utf8(<<16#C5>>)), %% CYRILLIC CAPITAL LETTER IE
		?_assertEqual(<<16#0416/utf8>>, uef_encode:win_to_utf8(<<16#C6>>)), %% CYRILLIC CAPITAL LETTER ZHE
		?_assertEqual(<<16#0417/utf8>>, uef_encode:win_to_utf8(<<16#C7>>)), %% CYRILLIC CAPITAL LETTER ZE
		?_assertEqual(<<16#0418/utf8>>, uef_encode:win_to_utf8(<<16#C8>>)), %% CYRILLIC CAPITAL LETTER I
		?_assertEqual(<<16#0419/utf8>>, uef_encode:win_to_utf8(<<16#C9>>)), %% CYRILLIC CAPITAL LETTER SHORT I
		?_assertEqual(<<16#041A/utf8>>, uef_encode:win_to_utf8(<<16#CA>>)), %% CYRILLIC CAPITAL LETTER KA
		?_assertEqual(<<16#041B/utf8>>, uef_encode:win_to_utf8(<<16#CB>>)), %% CYRILLIC CAPITAL LETTER EL
		?_assertEqual(<<16#041C/utf8>>, uef_encode:win_to_utf8(<<16#CC>>)), %% CYRILLIC CAPITAL LETTER EM
		?_assertEqual(<<16#041D/utf8>>, uef_encode:win_to_utf8(<<16#CD>>)), %% CYRILLIC CAPITAL LETTER EN
		?_assertEqual(<<16#041E/utf8>>, uef_encode:win_to_utf8(<<16#CE>>)), %% CYRILLIC CAPITAL LETTER O
		?_assertEqual(<<16#041F/utf8>>, uef_encode:win_to_utf8(<<16#CF>>)), %% CYRILLIC CAPITAL LETTER PE
		?_assertEqual(<<16#0420/utf8>>, uef_encode:win_to_utf8(<<16#D0>>)), %% CYRILLIC CAPITAL LETTER ER
		?_assertEqual(<<16#0421/utf8>>, uef_encode:win_to_utf8(<<16#D1>>)), %% CYRILLIC CAPITAL LETTER ES
		?_assertEqual(<<16#0422/utf8>>, uef_encode:win_to_utf8(<<16#D2>>)), %% CYRILLIC CAPITAL LETTER TE
		?_assertEqual(<<16#0423/utf8>>, uef_encode:win_to_utf8(<<16#D3>>)), %% CYRILLIC CAPITAL LETTER U
		?_assertEqual(<<16#0424/utf8>>, uef_encode:win_to_utf8(<<16#D4>>)), %% CYRILLIC CAPITAL LETTER EF
		?_assertEqual(<<16#0425/utf8>>, uef_encode:win_to_utf8(<<16#D5>>)), %% CYRILLIC CAPITAL LETTER HA
		?_assertEqual(<<16#0426/utf8>>, uef_encode:win_to_utf8(<<16#D6>>)), %% CYRILLIC CAPITAL LETTER TSE
		?_assertEqual(<<16#0427/utf8>>, uef_encode:win_to_utf8(<<16#D7>>)), %% CYRILLIC CAPITAL LETTER CHE
		?_assertEqual(<<16#0428/utf8>>, uef_encode:win_to_utf8(<<16#D8>>)), %% CYRILLIC CAPITAL LETTER SHA
		?_assertEqual(<<16#0429/utf8>>, uef_encode:win_to_utf8(<<16#D9>>)), %% CYRILLIC CAPITAL LETTER SHCHA
		?_assertEqual(<<16#042A/utf8>>, uef_encode:win_to_utf8(<<16#DA>>)), %% CYRILLIC CAPITAL LETTER HARD SIGN
		?_assertEqual(<<16#042B/utf8>>, uef_encode:win_to_utf8(<<16#DB>>)), %% CYRILLIC CAPITAL LETTER YERU
		?_assertEqual(<<16#042C/utf8>>, uef_encode:win_to_utf8(<<16#DC>>)), %% CYRILLIC CAPITAL LETTER SOFT SIGN
		?_assertEqual(<<16#042D/utf8>>, uef_encode:win_to_utf8(<<16#DD>>)), %% CYRILLIC CAPITAL LETTER E
		?_assertEqual(<<16#042E/utf8>>, uef_encode:win_to_utf8(<<16#DE>>)), %% CYRILLIC CAPITAL LETTER YU
		?_assertEqual(<<16#042F/utf8>>, uef_encode:win_to_utf8(<<16#DF>>)), %% CYRILLIC CAPITAL LETTER YA
		?_assertEqual(<<16#0430/utf8>>, uef_encode:win_to_utf8(<<16#E0>>)), %% CYRILLIC SMALL LETTER A
		?_assertEqual(<<16#0431/utf8>>, uef_encode:win_to_utf8(<<16#E1>>)), %% CYRILLIC SMALL LETTER BE
		?_assertEqual(<<16#0432/utf8>>, uef_encode:win_to_utf8(<<16#E2>>)), %% CYRILLIC SMALL LETTER VE
		?_assertEqual(<<16#0433/utf8>>, uef_encode:win_to_utf8(<<16#E3>>)), %% CYRILLIC SMALL LETTER GHE
		?_assertEqual(<<16#0434/utf8>>, uef_encode:win_to_utf8(<<16#E4>>)), %% CYRILLIC SMALL LETTER DE
		?_assertEqual(<<16#0435/utf8>>, uef_encode:win_to_utf8(<<16#E5>>)), %% CYRILLIC SMALL LETTER IE
		?_assertEqual(<<16#0436/utf8>>, uef_encode:win_to_utf8(<<16#E6>>)), %% CYRILLIC SMALL LETTER ZHE
		?_assertEqual(<<16#0437/utf8>>, uef_encode:win_to_utf8(<<16#E7>>)), %% CYRILLIC SMALL LETTER ZE
		?_assertEqual(<<16#0438/utf8>>, uef_encode:win_to_utf8(<<16#E8>>)), %% CYRILLIC SMALL LETTER I
		?_assertEqual(<<16#0439/utf8>>, uef_encode:win_to_utf8(<<16#E9>>)), %% CYRILLIC SMALL LETTER SHORT I
		?_assertEqual(<<16#043A/utf8>>, uef_encode:win_to_utf8(<<16#EA>>)), %% CYRILLIC SMALL LETTER KA
		?_assertEqual(<<16#043B/utf8>>, uef_encode:win_to_utf8(<<16#EB>>)), %% CYRILLIC SMALL LETTER EL
		?_assertEqual(<<16#043C/utf8>>, uef_encode:win_to_utf8(<<16#EC>>)), %% CYRILLIC SMALL LETTER EM
		?_assertEqual(<<16#043D/utf8>>, uef_encode:win_to_utf8(<<16#ED>>)), %% CYRILLIC SMALL LETTER EN
		?_assertEqual(<<16#043E/utf8>>, uef_encode:win_to_utf8(<<16#EE>>)), %% CYRILLIC SMALL LETTER O
		?_assertEqual(<<16#043F/utf8>>, uef_encode:win_to_utf8(<<16#EF>>)), %% CYRILLIC SMALL LETTER PE
		?_assertEqual(<<16#0440/utf8>>, uef_encode:win_to_utf8(<<16#F0>>)), %% CYRILLIC SMALL LETTER ER
		?_assertEqual(<<16#0441/utf8>>, uef_encode:win_to_utf8(<<16#F1>>)), %% CYRILLIC SMALL LETTER ES
		?_assertEqual(<<16#0442/utf8>>, uef_encode:win_to_utf8(<<16#F2>>)), %% CYRILLIC SMALL LETTER TE
		?_assertEqual(<<16#0443/utf8>>, uef_encode:win_to_utf8(<<16#F3>>)), %% CYRILLIC SMALL LETTER U
		?_assertEqual(<<16#0444/utf8>>, uef_encode:win_to_utf8(<<16#F4>>)), %% CYRILLIC SMALL LETTER EF
		?_assertEqual(<<16#0445/utf8>>, uef_encode:win_to_utf8(<<16#F5>>)), %% CYRILLIC SMALL LETTER HA
		?_assertEqual(<<16#0446/utf8>>, uef_encode:win_to_utf8(<<16#F6>>)), %% CYRILLIC SMALL LETTER TSE
		?_assertEqual(<<16#0447/utf8>>, uef_encode:win_to_utf8(<<16#F7>>)), %% CYRILLIC SMALL LETTER CHE
		?_assertEqual(<<16#0448/utf8>>, uef_encode:win_to_utf8(<<16#F8>>)), %% CYRILLIC SMALL LETTER SHA
		?_assertEqual(<<16#0449/utf8>>, uef_encode:win_to_utf8(<<16#F9>>)), %% CYRILLIC SMALL LETTER SHCHA
		?_assertEqual(<<16#044A/utf8>>, uef_encode:win_to_utf8(<<16#FA>>)), %% CYRILLIC SMALL LETTER HARD SIGN
		?_assertEqual(<<16#044B/utf8>>, uef_encode:win_to_utf8(<<16#FB>>)), %% CYRILLIC SMALL LETTER YERU
		?_assertEqual(<<16#044C/utf8>>, uef_encode:win_to_utf8(<<16#FC>>)), %% CYRILLIC SMALL LETTER SOFT SIGN
		?_assertEqual(<<16#044D/utf8>>, uef_encode:win_to_utf8(<<16#FD>>)), %% CYRILLIC SMALL LETTER E
		?_assertEqual(<<16#044E/utf8>>, uef_encode:win_to_utf8(<<16#FE>>)), %% CYRILLIC SMALL LETTER YU
		?_assertEqual(<<16#044F/utf8>>, uef_encode:win_to_utf8(<<16#FF>>))  %% CYRILLIC SMALL LETTER YA
	].
