# uef-lib
uef-lib is a Useful Erlang Functions Library. It can be used in OTP applications and contains some functions optimized for performance in specific cases (e.g. for file I/O operations or binary transformations).

## Modules
- **uef_bin** - for binaries.
- **uef_crypt** - some crypto functions.
- **uef_encode** - working with encodings.
- **uef_file** - working with files.
- **uef_format** - formatting numbers.
- **uef_lists** - lists transformations.
- **uef_num** - helpful functions for numbers.
- **uef_time** - date/time functions.

## Documentation

### Module `uef_bin`

<hr/>

#### *uef_bin:binary_join(ListOfBinaries, Separator) -> Binary.*<br/>
`uef_bin:binary_join/2` joins a list of binaries with separator into a single binary. Returns binary.

Example:
```erlang
> uef_bin:binary_join([<<"www">>, <<"example">>, <<"com">>], <<".">>).
<<"www.example.com">>
```

<hr/>

#### *uef_bin:split(Binary, Splitter) -> ListOfBinaries.*
`uef_bin:split/2` splits binary (`Binary`) with splitter (`Splitter`) into a list of binaries. Works as [binary:split/2](http://erlang.org/doc/man/binary.html#split-2) but is more performant in simple cases.

Examples:
```erlang
> uef_bin:split(<<".www.example.com.">>, <<".">>).
[<<>>,<<"www">>,<<"example">>,<<"com">>,<<>>]

> uef_bin:split(<<"www.example.com">>, <<".">>).
[<<"www">>,<<"example">>,<<"com">>]

> uef_bin:split(<<"www.example.com">>, <<"A">>).
[<<"www.example.com">>]
```

<hr/>

#### *uef_bin:split(Binary, Splitter, 'trim_all') -> ListOfBinaries.*
`uef_bin:split/3` splits binary (`Binary`) with splitter (`Splitter`) into a list of binaries. Works as `uef_bin:split/2` but removes all epmty (`<<>>`) chunks. It can be used in simple cases instead of  [binary:split/3](http://erlang.org/doc/man/binary.html#split-3) for the reason that it's more performant.

Example:
```erlang
> uef_bin:split(<<"..www.example.com.">>, <<".">>, trim_all).
[<<"www">>,<<"example">>,<<"com">>]
```

<hr/>

#### *uef_bin:replace(Binary1, Chars, OtherChars) -> Binary2.*
`uef_bin:replace/3` replaces chars (`Chars`) with other chars (`OtherChars`) in a binary (`Binary1`) and returns another binary (`Binary2`). Works as [binary:replace/3](http://erlang.org/doc/man/binary.html#replace-3) but more permormant and can be used in simple cases.

Examples:
```erlang
> uef_bin:replace(<<"abcdefgbc">>, <<"bc">>, <<"ZZ">>).
<<"aZZdefgZZ">>

> uef_bin:replace(<<"abcdefgbc">>, <<"d">>, <<"ZZ">>).
<<"abcZZefgbc">>
```

<hr/>

#### *uef_bin:replace_chars(Binary1, ListOfCharsToReplace, OtherChars) -> Binary2.*
`uef_bin:replace_chars/3` replaces chars inluded in list (`ListOfCharsToReplace`) with other chars (`OtherChars`) in a binary (`Binary1`) and returns another binary (`Binary2`).

Examples:
```erlang
uef_bin:replace_chars(<<"..www.example.com.">>, [<<".">>], <<>>).
<<"wwwexamplecom">>

uef_bin:replace_chars(<<"..www.example.com.">>, [<<".">>, <<"w">>], <<>>).
<<"examplecom">>
```

<hr/>

#### *uef_bin:random_latin_binary(Length, CaseFlag) -> RandomLatinBinary.*
`uef_bin:random_latin_binary/2` generates and returns a binary of size `Length` which consists of latins [a-z], [A-Z] or [a-zA-Z]. The second argument `CaseFlag` corresponds to a letter case, an atom `'lower'`, `'upper'` or `'any'`.

Examples:
```erlang
> uef_bin:random_latin_binary(10, lower).
<<"n0ui89sfsb">>

> uef_bin:random_latin_binary(10, upper).
<<"S11Y3DHEJI">>

> uef_bin:random_latin_binary(10, any).
<<"mTa9Lj7KUN">>
```

<hr/>

#### *uef_bin:random_binary_from_chars(Length, Chars) -> RandomCharsBinary.*
`uef_bin:random_binary_from_chars/2` generates and returns a binary of size `Length` which consists of the given characters `Chars`.

Example:
```erlang
> uef_bin:random_binary_from_chars(16, <<"ErlangForever">>).
<<"eFveerorreravgng">>
```

<hr/>

#### *uef_bin:numeric_prefix(Binary) -> DigitsOnlyOrEmptyBinary.*
`uef_bin:numeric_prefix/1` returns new binary (`DigitsOnlyBinary`) which consists of digits [0-9] wich are at the beginning in the given binary (`Binary`). If `Binary` does not begin with digit, this function returns empty binary (`<<>>`).

Examples:
```erlang
> uef_bin:numeric_prefix(<<"3456sld1knskjd">>).
<<"3456">>

> uef_bin:numeric_prefix(<<"ddd3456sld1knskjd">>).
<<>>
```

<hr/>

### Module `uef_crypt`
coming soon...

### Module `uef_encode`
coming soon...

### Module `uef_file`
coming soon...

### Module `uef_format`
coming soon...

### Module `uef_lists`
coming soon...

### Module `uef_num`
coming soon...

### Module `uef_time`
coming soon...

