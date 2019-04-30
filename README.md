# uef-lib

*uef-lib* is a **U**seful **E**rlang **F**unctions **Lib**rary. It can be used in OTP applications and contains some functions optimized for performance in specific cases (e.g. for file I/O operations or binary transformations).

See how to [build](#build) and [test](#test-and-dialyze).

## Modules

* **[uef_bin](#module-uef_bin)** - for binaries.
* **[uef_crypt](#module-uef_crypt)** - some crypto functions.
* **[uef_encode](#module-uef_encode)** - working with encodings.
* **[uef_file](#module-uef_file)** - working with files.
* **[uef_format](#module-uef_format)** - formatting numbers.
* **[uef_lists](#module-uef_lists)** - lists transformations.
* **[uef_num](#module-uef_num)** - helpful functions for numbers.
* **[uef_time](#module-uef_time)** - date/time functions.

## Documentation

### Module `uef_bin`

---

#### *uef_bin:binary_join/2*

```erlang
uef_bin:binary_join(ListOfBinaries, Separator) -> Binary.
```

Joins a list of binaries with separator into a single binary. Returns binary.

**Example:**

```erlang
> uef_bin:binary_join([<<"www">>, <<"example">>, <<"com">>], <<".">>).
<<"www.example.com">>
```

---

#### *uef_bin:reverse/1*

```erlang
uef_bin:reverse(Binary1) -> Binary2.
```

Returns a binary in reverse *byte* order.

**Note:** this function is **not** intended to work with UTF-8 binary strings. To get a binary in reverse *character* order, use [uef_bin:reverse_utf8/1](#uef_binreverse_utf81) instead.

**Examples:**

```erlang
> uef_bin:reverse(<<"ABCDEFGH">>).
<<"HGFEDCBA">>

> uef_bin:reverse(<<1,2,3,4,5>>).
<<5,4,3,2,1>>

> uef_bin:reverse(<<>>).
<<>>
```

---

#### *uef_bin:reverse_utf8/1*

```erlang
uef_bin:reverse_utf8(UTF8_Binary1) -> UTF8_Binary2.
```

Returns a binary in reverse character order. Intended to work with UTF-8 binary strings.

**Examples:**

```erlang
> uef_bin:reverse_utf8(<<"ABCDEFGH">>).
<<"HGFEDCBA">>

> uef_bin:reverse_utf8(<<1,2,3,4,5>>).
<<5,4,3,2,1>>

> uef_bin:reverse_utf8(<<"die Straße"/utf8>>).
<<"eßartS eid"/utf8>>

> uef_bin:reverse_utf8(<<"АБВГДЕЁЖ"/utf8>>) =:= <<"ЖЁЕДГВБА"/utf8>>.
true

> uef_bin:reverse_utf8(<<1, 2, 3, "АБВГДЕЁЖ"/utf8, 4, 5, 6, 7>>) =:= <<7, 6, 5, 4, "ЖЁЕДГВБА"/utf8, 3, 2, 1>>.
true

> uef_bin:reverse_utf8(<<"這條街"/utf8>>) =:= <<"街條這"/utf8>>.
true

> uef_bin:reverse_utf8(<<"こんにちは"/utf8>>) =:= <<"はちにんこ"/utf8>>.
true
```

---

#### *uef_bin:split/2*

```erlang
uef_bin:split(Binary, Splitter) -> ListOfBinaries.
```

Splits binary (`Binary`) with splitter (`Splitter`) into a list of binaries. Works as [binary:split/2](http://erlang.org/doc/man/binary.html#split-2) but is more performant in simple cases.

**Examples:**

```erlang
> uef_bin:split(<<".www.example.com.">>, <<".">>).
[<<>>,<<"www">>,<<"example">>,<<"com">>,<<>>]

> uef_bin:split(<<"www.example.com">>, <<".">>).
[<<"www">>,<<"example">>,<<"com">>]

> uef_bin:split(<<"www.example.com">>, <<"A">>).
[<<"www.example.com">>]
```

---

#### *uef_bin:split/3*

```erlang
uef_bin:split(Binary, Splitter, 'trim_all') -> ListOfBinaries.
```

Splits binary (`Binary`) with splitter (`Splitter`) into a list of binaries. Works as `uef_bin:split/2` but removes all epmty (`<<>>`) chunks. It can be used in simple cases instead of  [binary:split/3](http://erlang.org/doc/man/binary.html#split-3) for the reason that it's more performant.

**Example:**

```erlang
> uef_bin:split(<<"..www.example.com.">>, <<".">>, trim_all).
[<<"www">>,<<"example">>,<<"com">>]
```

---

#### *uef_bin:replace/3*

```erlang
uef_bin:replace(Binary1, Chars, OtherChars) -> Binary2.
```

Replaces chars (`Chars`) with other chars (`OtherChars`) in a binary (`Binary1`) and returns another binary (`Binary2`). Works as [binary:replace/3](http://erlang.org/doc/man/binary.html#replace-3) but more permormant and can be used in simple cases.

**Examples:**

```erlang
> uef_bin:replace(<<"abcdefgbc">>, <<"bc">>, <<"ZZ">>).
<<"aZZdefgZZ">>

> uef_bin:replace(<<"abcdefgbc">>, <<"d">>, <<"ZZ">>).
<<"abcZZefgbc">>
```

---

#### *uef_bin:replace_chars/3*

```erlang
uef_bin:replace_chars(Binary1, ListOfCharsToReplace, OtherChars) -> Binary2.
```

Replaces chars inluded in list (`ListOfCharsToReplace`) with other chars (`OtherChars`) in a binary (`Binary1`) and returns another binary (`Binary2`).

**Examples:**

```erlang
uef_bin:replace_chars(<<"..www.example.com.">>, [<<".">>], <<>>).
<<"wwwexamplecom">>

uef_bin:replace_chars(<<"..www.example.com.">>, [<<".">>, <<"w">>], <<>>).
<<"examplecom">>
```

---

#### *uef_bin:random_latin_binary/2*

```erlang
uef_bin:random_latin_binary(Length, CaseFlag) -> RandomLatinBinary.
```

Generates and returns a binary of size `Length` which consists of latins [a-z], [A-Z] or [a-zA-Z]. The second argument `CaseFlag` corresponds to a letter case, an atom `'lower'`, `'upper'` or `'any'`.

**Examples:**

```erlang
> uef_bin:random_latin_binary(10, lower).
<<"n0ui89sfsb">>

> uef_bin:random_latin_binary(10, upper).
<<"S11Y3DHEJI">>

> uef_bin:random_latin_binary(10, any).
<<"mTa9Lj7KUN">>
```

---

#### *uef_bin:random_binary_from_chars/2*

```erlang
uef_bin:random_binary_from_chars(Length, Chars) -> RandomCharsBinary.
```

Generates and returns a binary of size `Length` which consists of the given characters `Chars`.

**Example:**

```erlang
> uef_bin:random_binary_from_chars(16, <<"ErlangForever">>).
<<"eFveerorreravgng">>
```

---

#### *uef_bin:numeric_prefix/1*

```erlang
uef_bin:numeric_prefix(Binary) -> DigitsOnlyOrEmptyBinary.
```

Returns new binary (`DigitsOnlyBinary`) which consists of digits [0-9] wich are at the beginning in the given binary (`Binary`). If `Binary` does not begin with digit, this function returns empty binary (`<<>>`).

**Examples:**

```erlang
> uef_bin:numeric_prefix(<<"3456sld1knskjd">>).
<<"3456">>

> uef_bin:numeric_prefix(<<"ddd3456sld1knskjd">>).
<<>>
```

---

### Module `uef_crypt`

---

#### *uef_crypt:md5_hex/1*

```erlang
uef_crypt:md5_hex(IoData) -> Binary.
```

Returns binary (`Binary`) in hexadecimal form of md5 hash of the argument `IoData`.

**Examples:**

```erlang
> uef_crypt:md5_hex("abcd").
<<"e2fc714c4727ee9395f324cd2e7f331f">>

> uef_crypt:md5_hex(<<"привет"/utf8>>).
<<"608333adc72f545078ede3aad71bfe74">>

> uef_crypt:md5_hex(["how", ["is", ["it"]], "going", $?]).
<<"eb89df06495cef83e3ec185aefe81d0e">>
```

---

### Module `uef_encode`

---

#### *uef_encode:html_encode_bin/1*

```erlang
uef_encode:html_encode_bin(Html) -> EncodedBinary.
```

Takes argument `Html`, replaces some unsafe symbols with their appropriate HTML entities and returns binary.

**Examples:**

```erlang
> uef_encode:html_encode_bin("<>&©\n™").
<<"&lt;&gt;&amp;&copy;<br/>&trade;">>

> uef_encode:html_encode_bin("♦±Σ").
<<"&#9830;&plusmn;&Sigma;">>
```

---

#### *uef_encode:html_encode_list/1*

```erlang
uef_encode:html_encode_list(Html) -> EncodedList.
```

Works as `uef_encode:html_encode_bin/1` but returns list of binaries.

**Examples:**

```erlang
> uef_encode:html_encode_list("<>&©\n™").
[<<"&lt;">>,<<"&gt;">>,<<"&amp;">>,<<"&copy;">>,<<"<br/>">>,<<"&trade;">>]

> uef_encode:html_encode_list("♦±Σ").
[<<"&#9830;">>,<<"&plusmn;">>,<<"&Sigma;">>]
```

---

#### *uef_encode:win_to_utf8/1*

```erlang
uef_encode:win_to_utf8(Binary1251) -> BinaryUtf8.
```

Converts *cp1251* binary to *utf-8* binary.

**Example:**

```erlang
file_1251_to_utf8() ->
    File1251 = "1251.txt",
    FileUtf8 = "utf8.txt",
    {ok, Bin1251} = file:read_file(File1251),
    BinUtf8 = uef_encode:win_to_utf8(Bin1251), %converting
    file:write_file(FileUtf8, BinUtf8).
```

---

### Module `uef_file`

---

#### *uef_file:read_file_info_fast/1*

```erlang
uef_file:read_file_info_fast(Filename) -> {ok, FileInfo} | {error, Reason}.
```

Retrieves information about **local** file. Returns `{ok, FileInfo}` if successful, otherwise `{error, Reason}`. Works as [file:read_file_info/2](http://erlang.org/doc/man/file.html#read_file_info-2) but optimized for **local** files. This is a wrapper of:

`file:read_file_info(Filename, [raw, {time, posix}])`.

---

#### *uef_file:read_file_fast/1*

```erlang
uef_file:read_file_fast(Filename) -> {ok, BinaryData} | {error, Reason}.
```

Reads contents of **local** file `Filename` and returns `{ok, BinaryData}`, where `BinaryData` is a binary data object that contains the contents of `Filename`, or `{error, Reason}` if an error occurs. This function is optimized for reading contents of **local** files, as no Erlang process is used. It calls [file:open/2](http://erlang.org/doc/man/file.html#open-2) with options `[read, raw, binary]`.

---

### Module `uef_format`

---

#### *uef_format:format_number/3*

```erlang
uef_format:format_number(Number, Precision, Decimals) -> FormattedNumber.
```

The same as `uef_format:format_number/4` with `#{}` as the forth argument. See [uef_format:format_number/4](#uef_formatformat_number4) docs.

**Examples:**

```erlang
> uef_format:format_number(199.4567, 2, 3).
<<"199.460">>

>uef_format:format_number(199.4567, 1, 3).
<<"199.500">>

> uef_format:format_number(199.4567, 0, 4).
<<"199.0000">>

> uef_format:format_number(199.4567, -1, 2).
<<"200.00">>
```

---

#### *uef_format:format_number/4*

```erlang
uef_format:format_number(Number, Precision, Decimals, Options) -> FormattedNumber.
```

Formats `Number` by adding thousands separator between each set of 3 digits to the left of the decimal point, substituting `Decimals` for the decimal point, and rounding to the specified `Precision`. Returns a **binary** value.

**Types:**

```erlang
Number :: number().
Precision :: integer().
Decimals :: non_neg_integer().
FormattedNumber :: binary().
```

`Options` is a map:

```erlang
#{
    thousands_sep => binary() | string(), % Thousands separator
    decimal_point => binary() | string(), % Decimal point
    cur_symbol => binary() | string(), %% Currency symbol
    cur_pos => 'left' | 'right', % Currency position against price (left or right)
    cur_sep => binary() | string() % Separator between currency and price
}
```

**Note:** to get maximum performance use **binary** values for options `thousands_sep`, `decimal_point`, `cur_symbol` and `cur_sep` instead of strings.

**Examples:**

```erlang
> uef_format:format_number(1234567890.4567, 2, 2, #{}).
<<"1234567890.46">>

> uef_format:format_number(1234567890.4567, 2, 2, #{thousands_sep => ",", cur_symbol => "$"}).
<<"$1,234,567,890.46">>

> uef_format:format_number(1234567890.4567, 2, 2, #{
    thousands_sep => ",",
    cur_symbol => "USD",
    cur_sep => " ", % whitespace
    cur_pos => right}).
<<"1,234,567,890.46 USD">>

> uef_format:format_number(1234567890.4567, 2, 4, #{
    thousands_sep => ",",
    decimal_point => "==",
    cur_symbol => "USD",
    cur_sep => " ",
    cur_pos => left}).
<<"USD 1,234,567,890==4600">>

> uef_format:format_number(1234567890.4567, 2, 4, #{
    thousands_sep => <<",">>, % binary()
    decimal_point => <<".">>, % binary()
    cur_symbol => <<"USD">>, % binary()
    cur_sep => <<" ">>, % binary()
    cur_pos => left}).
<<"USD 1,234,567,890.4600">>
```

---

#### *uef_format:format_price/1*

```erlang
uef_format:format_price(Number) -> FormattedPrice.
```

Formats `Number` in price-like style. Returns a binary containing `FormattedPrice` formatted with a precision of `2` and decimal digits of `2`.

The same as `uef_format:format_price/2` with a precision of `2` as the second argument. See [uef_format:format_price/2](#uef_formatformat_price2) docs.

**Examples:**

```erlang
> uef_format:format_price(199).
<<"199.00">>

> uef_format:format_price(199.9876).
<<"199.99">>
```

---

#### *uef_format:format_price/2*

```erlang
uef_format:format_price(Number, Precision) -> FormattedPrice.
```

Formats `Number` in price-like style. Returns a binary containing `FormattedPrice` formatted with a specified precision as the second argument and decimal digits of `2`.

The same as `uef_format:format_price/3` with `#{}` as the third argument. See [uef_format:format_price/3](#uef_formatformat_price3) docs.

**Example:**

```erlang
> uef_format:format_price(1999.9876, 4).
<<"1999.99">>
```

---

#### *uef_format:format_price/3*

```erlang
uef_format:format_price(Number, Precision, CurrencySymbol_OR_Options) -> FormattedPrice.
```

Formats `Number` in price-like style. Returns a binary containing `FormattedPrice` formatted with a specified precision as the second argument, decimal digits of `2`, and with currency symbol (or options) as the third argument.

If `CurrencySymbol_OR_Options` is a `map` the functions works as [uef_format:format_number/4](#uef_formatformat_number4) with decimal digits of `2` as the third argument and with options as the forth one.

If `CurrencySymbol_OR_Options` is a `binary` or a `string`, the corresponding currency symbol is added to the left.

**Examples:**

```erlang
> uef_format:format_price(1000.8767, 4, #{}).
<<"1000.88">>


> uef_format:format_price(1000.8767, 4, #{
    thousands_sep => ",",
    cur_symbol => "USD",
    cur_sep => " ",
    cur_pos => right}).
<<"1,000.88 USD">>


> uef_format:format_price(1000.8767, 4, #{
    thousands_sep => ",",
    cur_symbol => <<"руб."/utf8>>,
    cur_sep => " ",
    cur_pos => right}).
<<49,44,48,48,48,46,56,56,32,209,128,209,131,208,177,46>> % <<"1,000.88 руб."/utf8>>.


> uef_format:format_price(1000.8767, 4, "$").
<<"$1000.88">>


> uef_format:format_price(99.999, 2, "$").
<<"$100.00">>


> uef_format:format_price(99.99, 2, "$").
<<"$99.99">>


> uef_format:format_price(99.99, 2, <<"€"/utf8>>).
<<226,130,172,57,57,46,57,57>> % <<"€99.99"/utf8>>

```

---

### Module `uef_lists`

---

#### *uef_lists:split_list_into_chunks/2*

```erlang
uef_lists:split_list_into_chunks(List, MaxLen) -> [List1, List2, ..., ListN].
```

Splits `List` into list of lists `[List1, List2, ..., ListN]` where `List1, List2, ..., ListN` are lists with maximum `MaxLen` elements.

**Examples:**

```erlang
> uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 1).
[[1],[2],[3],[4],[5],[6],[7],[8]]

> uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 2).
[[1,2],[3,4],[5,6],[7,8]]

> uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 3).
[[1,2,3],[4,5,6],[7,8]]

> uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 4).
[[1,2,3,4],[5,6,7,8]]

> uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 8).
[[1,2,3,4,5,6,7,8]]

> uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 9).
[[1,2,3,4,5,6,7,8]]

> uef_lists:split_list_into_chunks([1,2,3,4,5,6,7,8], 99).
[[1,2,3,4,5,6,7,8]]
```

---

#### *uef_lists:lists_to_list_of_tuples/2*

```erlang
uef_lists:lists_to_list_of_tuples(List1, List2) -> List3.
```

Transforms two lists into one list of two-tuples, where the first element of each tuple is taken from the first list and the second element is taken from the second list one by one.

**Examples:**

```erlang
> uef_lists:lists_to_list_of_tuples([a,b,c], [1,2]).
[{a,1},{a,2},{b,1},{b,2},{c,1},{c,2}]

> uef_lists:lists_to_list_of_tuples([a,b,c], [1,2,3]).
[{a,1},{a,2},{a,3},{b,1},{b,2},{b,3},{c,1},{c,2},{c,3}]
```

---

#### *uef_lists:lists_to_list_of_tuples/3*

```erlang
uef_lists:lists_to_list_of_tuples(List1, List2, List3) -> List4.
```

Transforms three lists into one list of three-tuples, where the first element of each tuple is taken from the first list, the second element is taken from the second list one by one, and the third element is taken from the third list one by one.

**Examples:**

```erlang
> uef_lists:lists_to_list_of_tuples([a1,b1], [a2,b2], [a3,b3]).
[{a1,a2,a3},
 {a1,a2,b3},
 {a1,b2,a3},
 {a1,b2,b3},
 {b1,a2,a3},
 {b1,a2,b3},
 {b1,b2,a3},
 {b1,b2,b3}]

> uef_lists:lists_to_list_of_tuples([a1,b1], [a2,b2,c2], [a3,b3]).
[{a1,a2,a3},
 {a1,a2,b3},
 {a1,b2,a3},
 {a1,b2,b3},
 {a1,c2,a3},
 {a1,c2,b3},
 {b1,a2,a3},
 {b1,a2,b3},
 {b1,b2,a3},
 {b1,b2,b3},
 {b1,c2,a3},
 {b1,c2,b3}]
```

---

### Module `uef_num`

---

#### *uef_num:round_number/2*

```erlang
uef_num:round_number(Number, Precision) -> Float.
```

Rounds the number to the specified precision.

**Examples:**

```erlang
> uef_num:round_number(10, 2).
10.0

> uef_num:round_number(123.786, 2).
123.79
```

---

#### *uef_num:round_price/1*

```erlang
uef_num:round_price(Number) -> Float.
```

Rounds the number to the precision of **2**. The same as `uef_num:round_number(Number, 2)`.

---

### Module `uef_time`

---

#### *uef_time:days_diff/1*

```erlang
uef_time:days_diff(Date) -> Days.
```

Returns the difference ***in days*** between `Date` and the current local date provided by function [erlang:localtime()](http://erlang.org/doc/man/erlang.html#localtime-0). `Date` must be of type `calendar:date()` (`{Year, Month, Day}`).

---

#### *uef_time:days_diff/2*

```erlang
uef_time:days_diff(Date1, Date2) -> Days.
```

Returns the difference ***in days*** between `Date2` and `Date1`. `Date1` and `Date2` must be of type `calendar:date()` (`{Year, Month, Day}`).

**Examples:**

```erlang
> uef_time:days_diff({1999, 1, 31}, {2019, 12, 31}).
7639

> uef_time:days_diff({2019, 12, 31}, {1999, 1, 31}).
-7639
```

---

#### *uef_time:seconds_diff/1*

```erlang
uef_time:seconds_diff(DateTime) -> Seconds.
```

Returns the difference ***in seconds*** between `Date` and the current local time provided by function [erlang:localtime()](http://erlang.org/doc/man/erlang.html#localtime-0). `DateTime` must be of type `calendar:datetime()` (`{{Year, Month, Day}, {Hour, Minute, Second}}`).

---

#### *uef_time:seconds_diff/2*

```erlang
uef_time:seconds_diff(DateTime1, DateTime2) -> Seconds.
```

Returns the difference ***in seconds*** between `DateTime2` and `DateTime1`.  `DateTime1` and `DateTime2` must be of type `calendar:datetime()` (`{{Year, Month, Day}, {Hour, Minute, Second}}`).

**Examples:**

```erlang
> uef_time:seconds_diff({{1999, 1, 31}, {0, 0, 0}}, {{2019, 12, 31}, {0, 0, 0}}).
660009600

> uef_time:seconds_diff({{2019, 12, 31}, {0, 0, 0}}, {{1999, 1, 31}, {0, 0, 0}}).
-660009600
```

---

## Build

### Build with GNU `make`

```bash
make
```

### Build with `rebar3`

```bash
rebar3 compile
```

## Test and dialyze

### Test/dialyze with GNU `make`

```bash
make test
```

```bash
make dialyzer
```

### Test/dialyze with `rebar3`

```bash
rebar3 eunit
```

```bash
rebar3 dialyzer
```

## Contributing

You are welcome :)
