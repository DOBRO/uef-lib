-module(uef_encode).

-export([html_encode_list/1, html_encode_bin/1]).
-export([win_to_utf8/1]).

%% html_encode_list/1
-spec html_encode_list(Html::iodata()) -> list().
html_encode_list(<<>>) -> [];
html_encode_list([]) -> [];
html_encode_list(Html) ->
	case unicode:characters_to_list(Html) of
		{incomplete, Encoded, _} ->
			html_encode_list(Encoded, []);
		{error, Encoded, _} ->
			html_encode_list(Encoded, []);
		List ->
			html_encode_list(List, [])
	end.

%% html_encode_bin/1
-spec html_encode_bin(Html::iodata()) -> binary().
html_encode_bin(<<>>) -> <<>>;
html_encode_bin([]) -> <<>>;
html_encode_bin(Html) -> unicode:characters_to_binary(html_encode_list(Html)).

%% win_to_utf8/1
-spec win_to_utf8(binary()) -> binary().
win_to_utf8(Bin) ->
	win_to_utf8(Bin, <<>>).

%%%%%%%%%%%%% Internal functions   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% html_encode/2
html_encode_list([], Acc) -> lists:reverse(Acc);
html_encode_list([H|T], Acc) -> html_encode_list(T, [html_encode_char(H)|Acc]).

%% html_encode_char/1
html_encode_char($\n) -> <<"<br/>">>;
html_encode_char($\r) -> <<>>;
html_encode_char($\t) -> <<" ">>;
html_encode_char($") -> <<"&quot;">>;
html_encode_char($—) -> <<"&mdash;">>;
html_encode_char($|) -> <<"&#124;">>;
html_encode_char($/) -> <<"&#47;">>;
html_encode_char($[) -> <<"&#91;">>;
html_encode_char($]) -> <<"&#93;">>;
html_encode_char(${) -> <<"&#123;">>;
html_encode_char($}) -> <<"&#125;">>;
html_encode_char($&) -> <<"&amp;">>;
html_encode_char($<) -> <<"&lt;">>;
html_encode_char($>) -> <<"&gt;">>;
html_encode_char($') -> <<"&#39;">>;
html_encode_char($%) -> <<"&#37;">>;
html_encode_char($#) -> <<"&#35;">>;
html_encode_char($№) -> <<"&#8470;">>;
html_encode_char($@) -> <<"&#64;">>;
html_encode_char($~) -> <<"&#126;">>;
html_encode_char($•) -> <<"&#8226;">>;
html_encode_char($○) -> <<"&#9675;">>;
html_encode_char($●) -> <<"&#9679;">>;
html_encode_char($™) -> <<"&trade;">>;
html_encode_char($©) -> <<"&copy;">>;
html_encode_char($®) -> <<"&reg;">>;
html_encode_char($¶) -> <<"&para;">>;
html_encode_char($♢) -> <<"&#9826;">>;
html_encode_char($♦) -> <<"&#9830;">>;
html_encode_char($$) -> <<"&#36;">>;
html_encode_char($€) -> <<"&euro;">>;
html_encode_char($«) -> <<"&laquo;">>;
html_encode_char($») -> <<"&raquo;">>;
html_encode_char($„) -> <<"&bdquo;">>;
html_encode_char($‚) -> <<"&sbquo;">>;
html_encode_char($`) -> <<"&#96;">>;
html_encode_char($°) -> <<"&deg;">>;
html_encode_char($±) -> <<"&plusmn;">>;
html_encode_char($´) -> <<"&#180;">>;
html_encode_char($‘) -> <<"&lsquo;">>;
html_encode_char($’) -> <<"&rsquo;">>;
html_encode_char($“) -> <<"&ldquo;">>;
html_encode_char($”) -> <<"&rdquo;">>;
html_encode_char($‹) -> <<"&lsaquo;">>;
html_encode_char($›) -> <<"&rsaquo;">>;
html_encode_char($→) -> <<"&rarr;">>;
html_encode_char($←) -> <<"&larr;">>;
html_encode_char($↑) -> <<"&uarr;">>;
html_encode_char($↓) -> <<"&darr;">>;
html_encode_char($↔) -> <<"&harr;">>;
html_encode_char($…) -> <<"&hellip;">>;
html_encode_char($§) -> <<"&sect;">>;
html_encode_char($µ) -> <<"&micro;">>;
html_encode_char($·) -> <<"&middot;">>;
html_encode_char($º) -> <<"&ordm;">>;
html_encode_char($÷) -> <<"&divide;">>;
html_encode_char($ο) -> <<"&omicron;">>;
html_encode_char($Ο) -> <<"&Omicron;">>;
html_encode_char($σ) -> <<"&sigma;">>;
html_encode_char($Σ) -> <<"&Sigma;">>;
html_encode_char($ω) -> <<"&omega;">>;
html_encode_char($Ω) -> <<"&Omega;">>;
html_encode_char($¬) -> <<"&not;">>;
html_encode_char($¦) -> <<"&#166;">>;
html_encode_char($∞) -> <<"&infin;">>;
html_encode_char($¹) -> <<"&sup1;">>;
html_encode_char($²) -> <<"&sup2;">>;
html_encode_char($³) -> <<"&sup3;">>;
html_encode_char($½) -> <<"&frac12;">>;
html_encode_char($⅓) -> <<"&#8531;">>;
html_encode_char($¼) -> <<"&frac14;">>;
html_encode_char(C) -> C.

%% win_to_utf8/2
win_to_utf8(<<C:8, Rest/binary>>, Acc) ->
	U = case C of
		16#00 -> 16#0000; %% NULL
		16#01 -> 16#0001; %% START OF HEADING
		16#02 -> 16#0002; %% START OF TEXT
		16#03 -> 16#0003; %% END OF TEXT
		16#04 -> 16#0004; %% END OF TRANSMISSION
		16#05 -> 16#0005; %% ENQUIRY
		16#06 -> 16#0006; %% ACKNOWLEDGE
		16#07 -> 16#0007; %% BELL
		16#08 -> 16#0008; %% BACKSPACE
		16#09 -> 16#0009; %% HORIZONTAL TABULATION
		16#0A -> 16#000A; %% LINE FEED
		16#0B -> 16#000B; %% VERTICAL TABULATION
		16#0C -> 16#000C; %% FORM FEED
		16#0D -> 16#000D; %% CARRIAGE RETURN
		16#0E -> 16#000E; %% SHIFT OUT
		16#0F -> 16#000F; %% SHIFT IN
		16#10 -> 16#0010; %% DATA LINK ESCAPE
		16#11 -> 16#0011; %% DEVICE CONTROL ONE
		16#12 -> 16#0012; %% DEVICE CONTROL TWO
		16#13 -> 16#0013; %% DEVICE CONTROL THREE
		16#14 -> 16#0014; %% DEVICE CONTROL FOUR
		16#15 -> 16#0015; %% NEGATIVE ACKNOWLEDGE
		16#16 -> 16#0016; %% SYNCHRONOUS IDLE
		16#17 -> 16#0017; %% END OF TRANSMISSION BLOCK
		16#18 -> 16#0018; %% CANCEL
		16#19 -> 16#0019; %% END OF MEDIUM
		16#1A -> 16#001A; %% SUBSTITUTE
		16#1B -> 16#001B; %% ESCAPE
		16#1C -> 16#001C; %% FILE SEPARATOR
		16#1D -> 16#001D; %% GROUP SEPARATOR
		16#1E -> 16#001E; %% RECORD SEPARATOR
		16#1F -> 16#001F; %% UNIT SEPARATOR
		16#20 -> 16#0020; %% SPACE
		16#21 -> 16#0021; %% EXCLAMATION MARK
		16#22 -> 16#0022; %% QUOTATION MARK
		16#23 -> 16#0023; %% NUMBER SIGN
		16#24 -> 16#0024; %% DOLLAR SIGN
		16#25 -> 16#0025; %% PERCENT SIGN
		16#26 -> 16#0026; %% AMPERSAND
		16#27 -> 16#0027; %% APOSTROPHE
		16#28 -> 16#0028; %% LEFT PARENTHESIS
		16#29 -> 16#0029; %% RIGHT PARENTHESIS
		16#2A -> 16#002A; %% ASTERISK
		16#2B -> 16#002B; %% PLUS SIGN
		16#2C -> 16#002C; %% COMMA
		16#2D -> 16#002D; %% HYPHEN-MINUS
		16#2E -> 16#002E; %% FULL STOP
		16#2F -> 16#002F; %% SOLIDUS
		16#30 -> 16#0030; %% DIGIT ZERO
		16#31 -> 16#0031; %% DIGIT ONE
		16#32 -> 16#0032; %% DIGIT TWO
		16#33 -> 16#0033; %% DIGIT THREE
		16#34 -> 16#0034; %% DIGIT FOUR
		16#35 -> 16#0035; %% DIGIT FIVE
		16#36 -> 16#0036; %% DIGIT SIX
		16#37 -> 16#0037; %% DIGIT SEVEN
		16#38 -> 16#0038; %% DIGIT EIGHT
		16#39 -> 16#0039; %% DIGIT NINE
		16#3A -> 16#003A; %% COLON
		16#3B -> 16#003B; %% SEMICOLON
		16#3C -> 16#003C; %% LESS-THAN SIGN
		16#3D -> 16#003D; %% EQUALS SIGN
		16#3E -> 16#003E; %% GREATER-THAN SIGN
		16#3F -> 16#003F; %% QUESTION MARK
		16#40 -> 16#0040; %% COMMERCIAL AT
		16#41 -> 16#0041; %% LATIN CAPITAL LETTER A
		16#42 -> 16#0042; %% LATIN CAPITAL LETTER B
		16#43 -> 16#0043; %% LATIN CAPITAL LETTER C
		16#44 -> 16#0044; %% LATIN CAPITAL LETTER D
		16#45 -> 16#0045; %% LATIN CAPITAL LETTER E
		16#46 -> 16#0046; %% LATIN CAPITAL LETTER F
		16#47 -> 16#0047; %% LATIN CAPITAL LETTER G
		16#48 -> 16#0048; %% LATIN CAPITAL LETTER H
		16#49 -> 16#0049; %% LATIN CAPITAL LETTER I
		16#4A -> 16#004A; %% LATIN CAPITAL LETTER J
		16#4B -> 16#004B; %% LATIN CAPITAL LETTER K
		16#4C -> 16#004C; %% LATIN CAPITAL LETTER L
		16#4D -> 16#004D; %% LATIN CAPITAL LETTER M
		16#4E -> 16#004E; %% LATIN CAPITAL LETTER N
		16#4F -> 16#004F; %% LATIN CAPITAL LETTER O
		16#50 -> 16#0050; %% LATIN CAPITAL LETTER P
		16#51 -> 16#0051; %% LATIN CAPITAL LETTER Q
		16#52 -> 16#0052; %% LATIN CAPITAL LETTER R
		16#53 -> 16#0053; %% LATIN CAPITAL LETTER S
		16#54 -> 16#0054; %% LATIN CAPITAL LETTER T
		16#55 -> 16#0055; %% LATIN CAPITAL LETTER U
		16#56 -> 16#0056; %% LATIN CAPITAL LETTER V
		16#57 -> 16#0057; %% LATIN CAPITAL LETTER W
		16#58 -> 16#0058; %% LATIN CAPITAL LETTER X
		16#59 -> 16#0059; %% LATIN CAPITAL LETTER Y
		16#5A -> 16#005A; %% LATIN CAPITAL LETTER Z
		16#5B -> 16#005B; %% LEFT SQUARE BRACKET
		16#5C -> 16#005C; %% REVERSE SOLIDUS
		16#5D -> 16#005D; %% RIGHT SQUARE BRACKET
		16#5E -> 16#005E; %% CIRCUMFLEX ACCENT
		16#5F -> 16#005F; %% LOW LINE
		16#60 -> 16#0060; %% GRAVE ACCENT
		16#61 -> 16#0061; %% LATIN SMALL LETTER A
		16#62 -> 16#0062; %% LATIN SMALL LETTER B
		16#63 -> 16#0063; %% LATIN SMALL LETTER C
		16#64 -> 16#0064; %% LATIN SMALL LETTER D
		16#65 -> 16#0065; %% LATIN SMALL LETTER E
		16#66 -> 16#0066; %% LATIN SMALL LETTER F
		16#67 -> 16#0067; %% LATIN SMALL LETTER G
		16#68 -> 16#0068; %% LATIN SMALL LETTER H
		16#69 -> 16#0069; %% LATIN SMALL LETTER I
		16#6A -> 16#006A; %% LATIN SMALL LETTER J
		16#6B -> 16#006B; %% LATIN SMALL LETTER K
		16#6C -> 16#006C; %% LATIN SMALL LETTER L
		16#6D -> 16#006D; %% LATIN SMALL LETTER M
		16#6E -> 16#006E; %% LATIN SMALL LETTER N
		16#6F -> 16#006F; %% LATIN SMALL LETTER O
		16#70 -> 16#0070; %% LATIN SMALL LETTER P
		16#71 -> 16#0071; %% LATIN SMALL LETTER Q
		16#72 -> 16#0072; %% LATIN SMALL LETTER R
		16#73 -> 16#0073; %% LATIN SMALL LETTER S
		16#74 -> 16#0074; %% LATIN SMALL LETTER T
		16#75 -> 16#0075; %% LATIN SMALL LETTER U
		16#76 -> 16#0076; %% LATIN SMALL LETTER V
		16#77 -> 16#0077; %% LATIN SMALL LETTER W
		16#78 -> 16#0078; %% LATIN SMALL LETTER X
		16#79 -> 16#0079; %% LATIN SMALL LETTER Y
		16#7A -> 16#007A; %% LATIN SMALL LETTER Z
		16#7B -> 16#007B; %% LEFT CURLY BRACKET
		16#7C -> 16#007C; %% VERTICAL LINE
		16#7D -> 16#007D; %% RIGHT CURLY BRACKET
		16#7E -> 16#007E; %% TILDE
		16#7F -> 16#007F; %% DELETE
		16#80 -> 16#0402; %% CYRILLIC CAPITAL LETTER DJE
		16#81 -> 16#0403; %% CYRILLIC CAPITAL LETTER GJE
		16#82 -> 16#201A; %% SINGLE LOW-9 QUOTATION MARK
		16#83 -> 16#0453; %% CYRILLIC SMALL LETTER GJE
		16#84 -> 16#201E; %% DOUBLE LOW-9 QUOTATION MARK
		16#85 -> 16#2026; %% HORIZONTAL ELLIPSIS
		16#86 -> 16#2020; %% DAGGER
		16#87 -> 16#2021; %% DOUBLE DAGGER
		16#88 -> 16#20AC; %% EURO SIGN
		16#89 -> 16#2030; %% PER MILLE SIGN
		16#8A -> 16#0409; %% CYRILLIC CAPITAL LETTER LJE
		16#8B -> 16#2039; %% SINGLE LEFT-POINTING ANGLE QUOTATION MARK
		16#8C -> 16#040A; %% CYRILLIC CAPITAL LETTER NJE
		16#8D -> 16#040C; %% CYRILLIC CAPITAL LETTER KJE
		16#8E -> 16#040B; %% CYRILLIC CAPITAL LETTER TSHE
		16#8F -> 16#040F; %% CYRILLIC CAPITAL LETTER DZHE
		16#90 -> 16#0452; %% CYRILLIC SMALL LETTER DJE
		16#91 -> 16#2018; %% LEFT SINGLE QUOTATION MARK
		16#92 -> 16#2019; %% RIGHT SINGLE QUOTATION MARK
		16#93 -> 16#201C; %% LEFT DOUBLE QUOTATION MARK
		16#94 -> 16#201D; %% RIGHT DOUBLE QUOTATION MARK
		16#95 -> 16#2022; %% BULLET
		16#96 -> 16#2013; %% EN DASH
		16#97 -> 16#2014; %% EM DASH
		16#99 -> 16#2122; %% TRADE MARK SIGN
		16#9A -> 16#0459; %% CYRILLIC SMALL LETTER LJE
		16#9B -> 16#203A; %% SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
		16#9C -> 16#045A; %% CYRILLIC SMALL LETTER NJE
		16#9D -> 16#045C; %% CYRILLIC SMALL LETTER KJE
		16#9E -> 16#045B; %% CYRILLIC SMALL LETTER TSHE
		16#9F -> 16#045F; %% CYRILLIC SMALL LETTER DZHE
		16#A0 -> 16#00A0; %% NO-BREAK SPACE
		16#A1 -> 16#040E; %% CYRILLIC CAPITAL LETTER SHORT U
		16#A2 -> 16#045E; %% CYRILLIC SMALL LETTER SHORT U
		16#A3 -> 16#0408; %% CYRILLIC CAPITAL LETTER JE
		16#A4 -> 16#00A4; %% CURRENCY SIGN
		16#A5 -> 16#0490; %% CYRILLIC CAPITAL LETTER GHE WITH UPTURN
		16#A6 -> 16#00A6; %% BROKEN BAR
		16#A7 -> 16#00A7; %% SECTION SIGN
		16#A8 -> 16#0401; %% CYRILLIC CAPITAL LETTER IO
		16#A9 -> 16#00A9; %% COPYRIGHT SIGN
		16#AA -> 16#0404; %% CYRILLIC CAPITAL LETTER UKRAINIAN IE
		16#AB -> 16#00AB; %% LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
		16#AC -> 16#00AC; %% NOT SIGN
		16#AD -> 16#00AD; %% SOFT HYPHEN
		16#AE -> 16#00AE; %% REGISTERED SIGN
		16#AF -> 16#0407; %% CYRILLIC CAPITAL LETTER YI
		16#B0 -> 16#00B0; %% DEGREE SIGN
		16#B1 -> 16#00B1; %% PLUS-MINUS SIGN
		16#B2 -> 16#0406; %% CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
		16#B3 -> 16#0456; %% CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
		16#B4 -> 16#0491; %% CYRILLIC SMALL LETTER GHE WITH UPTURN
		16#B5 -> 16#00B5; %% MICRO SIGN
		16#B6 -> 16#00B6; %% PILCROW SIGN
		16#B7 -> 16#00B7; %% MIDDLE DOT
		16#B8 -> 16#0451; %% CYRILLIC SMALL LETTER IO
		16#B9 -> 16#2116; %% NUMERO SIGN
		16#BA -> 16#0454; %% CYRILLIC SMALL LETTER UKRAINIAN IE
		16#BB -> 16#00BB; %% RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
		16#BC -> 16#0458; %% CYRILLIC SMALL LETTER JE
		16#BD -> 16#0405; %% CYRILLIC CAPITAL LETTER DZE
		16#BE -> 16#0455; %% CYRILLIC SMALL LETTER DZE
		16#BF -> 16#0457; %% CYRILLIC SMALL LETTER YI
		16#C0 -> 16#0410; %% CYRILLIC CAPITAL LETTER A
		16#C1 -> 16#0411; %% CYRILLIC CAPITAL LETTER BE
		16#C2 -> 16#0412; %% CYRILLIC CAPITAL LETTER VE
		16#C3 -> 16#0413; %% CYRILLIC CAPITAL LETTER GHE
		16#C4 -> 16#0414; %% CYRILLIC CAPITAL LETTER DE
		16#C5 -> 16#0415; %% CYRILLIC CAPITAL LETTER IE
		16#C6 -> 16#0416; %% CYRILLIC CAPITAL LETTER ZHE
		16#C7 -> 16#0417; %% CYRILLIC CAPITAL LETTER ZE
		16#C8 -> 16#0418; %% CYRILLIC CAPITAL LETTER I
		16#C9 -> 16#0419; %% CYRILLIC CAPITAL LETTER SHORT I
		16#CA -> 16#041A; %% CYRILLIC CAPITAL LETTER KA
		16#CB -> 16#041B; %% CYRILLIC CAPITAL LETTER EL
		16#CC -> 16#041C; %% CYRILLIC CAPITAL LETTER EM
		16#CD -> 16#041D; %% CYRILLIC CAPITAL LETTER EN
		16#CE -> 16#041E; %% CYRILLIC CAPITAL LETTER O
		16#CF -> 16#041F; %% CYRILLIC CAPITAL LETTER PE
		16#D0 -> 16#0420; %% CYRILLIC CAPITAL LETTER ER
		16#D1 -> 16#0421; %% CYRILLIC CAPITAL LETTER ES
		16#D2 -> 16#0422; %% CYRILLIC CAPITAL LETTER TE
		16#D3 -> 16#0423; %% CYRILLIC CAPITAL LETTER U
		16#D4 -> 16#0424; %% CYRILLIC CAPITAL LETTER EF
		16#D5 -> 16#0425; %% CYRILLIC CAPITAL LETTER HA
		16#D6 -> 16#0426; %% CYRILLIC CAPITAL LETTER TSE
		16#D7 -> 16#0427; %% CYRILLIC CAPITAL LETTER CHE
		16#D8 -> 16#0428; %% CYRILLIC CAPITAL LETTER SHA
		16#D9 -> 16#0429; %% CYRILLIC CAPITAL LETTER SHCHA
		16#DA -> 16#042A; %% CYRILLIC CAPITAL LETTER HARD SIGN
		16#DB -> 16#042B; %% CYRILLIC CAPITAL LETTER YERU
		16#DC -> 16#042C; %% CYRILLIC CAPITAL LETTER SOFT SIGN
		16#DD -> 16#042D; %% CYRILLIC CAPITAL LETTER E
		16#DE -> 16#042E; %% CYRILLIC CAPITAL LETTER YU
		16#DF -> 16#042F; %% CYRILLIC CAPITAL LETTER YA
		16#E0 -> 16#0430; %% CYRILLIC SMALL LETTER A
		16#E1 -> 16#0431; %% CYRILLIC SMALL LETTER BE
		16#E2 -> 16#0432; %% CYRILLIC SMALL LETTER VE
		16#E3 -> 16#0433; %% CYRILLIC SMALL LETTER GHE
		16#E4 -> 16#0434; %% CYRILLIC SMALL LETTER DE
		16#E5 -> 16#0435; %% CYRILLIC SMALL LETTER IE
		16#E6 -> 16#0436; %% CYRILLIC SMALL LETTER ZHE
		16#E7 -> 16#0437; %% CYRILLIC SMALL LETTER ZE
		16#E8 -> 16#0438; %% CYRILLIC SMALL LETTER I
		16#E9 -> 16#0439; %% CYRILLIC SMALL LETTER SHORT I
		16#EA -> 16#043A; %% CYRILLIC SMALL LETTER KA
		16#EB -> 16#043B; %% CYRILLIC SMALL LETTER EL
		16#EC -> 16#043C; %% CYRILLIC SMALL LETTER EM
		16#ED -> 16#043D; %% CYRILLIC SMALL LETTER EN
		16#EE -> 16#043E; %% CYRILLIC SMALL LETTER O
		16#EF -> 16#043F; %% CYRILLIC SMALL LETTER PE
		16#F0 -> 16#0440; %% CYRILLIC SMALL LETTER ER
		16#F1 -> 16#0441; %% CYRILLIC SMALL LETTER ES
		16#F2 -> 16#0442; %% CYRILLIC SMALL LETTER TE
		16#F3 -> 16#0443; %% CYRILLIC SMALL LETTER U
		16#F4 -> 16#0444; %% CYRILLIC SMALL LETTER EF
		16#F5 -> 16#0445; %% CYRILLIC SMALL LETTER HA
		16#F6 -> 16#0446; %% CYRILLIC SMALL LETTER TSE
		16#F7 -> 16#0447; %% CYRILLIC SMALL LETTER CHE
		16#F8 -> 16#0448; %% CYRILLIC SMALL LETTER SHA
		16#F9 -> 16#0449; %% CYRILLIC SMALL LETTER SHCHA
		16#FA -> 16#044A; %% CYRILLIC SMALL LETTER HARD SIGN
		16#FB -> 16#044B; %% CYRILLIC SMALL LETTER YERU
		16#FC -> 16#044C; %% CYRILLIC SMALL LETTER SOFT SIGN
		16#FD -> 16#044D; %% CYRILLIC SMALL LETTER E
		16#FE -> 16#044E; %% CYRILLIC SMALL LETTER YU
		16#FF -> 16#044F; %% CYRILLIC SMALL LETTER YA
		Other -> Other
	end,
	win_to_utf8(Rest, <<Acc/binary, U/utf8>>);
win_to_utf8(<<>>, Acc) ->
	Acc.


