# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

- License changed to Apache 2.0.
- README converted to asciidoc format.

## [2.5.1] - 2019-09-26

- Some binary optimizations in `uef_bin` and `uef_format` modules.

## [2.5.0] - 2019-08-09

- Separator option `sep` added for function `uef_format:format_bytes/2`.

## [2.4.0] - 2019-07-28

- New function `uef_lists:search/2`.

## [2.3.0] - 2019-07-10

- New function `uef_format:format_bytes/1`.
- New function `uef_format:format_bytes/2`.

## [2.2.0] - 2019-06-28

- New function `uef_num:ctz/1`.
- New function `uef_num:lsb_pos/1`.
- New function `uef_num:msb_pos/1`.

## [2.1.0] - 2019-06-24

- New function `uef_num:popcount/1`.

## [2.0.5] - 2019-05-31

- Fixed: call of function `uef_format:format_price/3` could fail with exception when currency symbol or separator was an UTF-8 string (not binary).

## [2.0.4] - 2019-05-30

- Fixed: `{get_warnings, true}` in rebar.config was the reason of high load when building PLT.
- `xref` options added to rebar.config.
- `cover` options added to rebar.config.

## [2.0.3] - 2019-05-30

- Strict options added to rebar.config.
- Fixed some typespecs and dialyzer warnings.

## [2.0.2] - 2019-05-29

- Edoc tags added to all modules.

## [2.0.1] - 2019-05-22

- Fixed: `uef.app.src` did not contain `uef_maps` module.

## [2.0.0] - 2019-05-19

- OTP-19 or higher is now required.
- New module `uef_maps`.
- New function `uef_maps:delete_nested/2`.
- New function `uef_maps:find_nested/2`.
- New function `uef_maps:get_nested/2`.
- New function `uef_maps:get_nested/3`.
- New function `uef_maps:is_key_nested/2`.
- New function `uef_maps:new_nested/1`.
- New function `uef_maps:new_nested/2`.
- New function `uef_maps:put_nested/3`.
- New function `uef_maps:update_nested/3`.
- New function `uef_maps:remove_nested/2`.
- New function `uef_maps:take_nested/2`.

## [1.6.1] - 2019-05-12

- Fixed: function `uef_time:unix_time/0` did not work with OTP-18 and OTP-19.
- OTP-18 or higher is now required.

## [1.6.0] - 2019-05-12

- New function `uef_time:unix_time/0`.
- New function `uef_time:unix_time/1`.

## [1.5.0] - 2019-05-12

- New function `uef_time:today/0`.
- New function `uef_time:tomorrow/0`.
- New function `uef_time:yesterday/0`.

## [1.4.0] - 2019-05-11

- New functions `uef_time:add_seconds/1,2`.
- New functions `uef_time:add_minutes/1,2`.
- New functions `uef_time:add_hours/1,2`.
- New functions `uef_time:add_days/1,2`.
- New functions `uef_time:add_weeks/1,2`.
- New functions `uef_time:add_months/1,2`.
- New functions `uef_time:add_years/1,2`.
- New functions `uef_time:add_time/1,2`.

## [1.3.0] - 2019-05-03

- New function `uef_bin:repeat/2`.

## [1.2.0] - 2019-04-30

- New function `uef_bin:reverse_utf8/1` which returns a binary in reverse character order.

## [1.1.0] - 2019-04-29

- New function `uef_bin:reverse/1` which returns a binary in reverse byte order.

## [1.0.0] - 2019-04-29

- New function `uef_format:format_number/3,4`. Supports custom precision, number of decimal digits, thousands separator, decimal point, currency symbols and custom currency position (left/right).
- New function `uef_format:format_price/3` based on `uef_format:format_number/3`.
- Module `uef_format` totally reworked.
- Function `uef_format:format_price/1,2` now returns other value, see README.

## [0.1.0] - 2019-04-24

- Initial release.
