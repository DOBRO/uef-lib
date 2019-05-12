# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.6.0] - 2019-05-12

- New function `uef_time:unix_time/0`
- New function `uef_time:unix_time/1`

## [1.5.0] - 2019-05-12

### Added

- New function `uef_time:today/0`
- New function `uef_time:tomorrow/0`
- New function `uef_time:yesterday/0`

## [1.4.0] - 2019-05-11

### Added

- New functions `uef_time:add_seconds/1,2`
- New functions `uef_time:add_minutes/1,2`
- New functions `uef_time:add_hours/1,2`
- New functions `uef_time:add_days/1,2`
- New functions `uef_time:add_weeks/1,2`
- New functions `uef_time:add_months/1,2`
- New functions `uef_time:add_years/1,2`
- New functions `uef_time:add_time/1,2`

## [1.3.0] - 2019-05-03

### Added

- New function `uef_bin:repeat/2`

## [1.2.0] - 2019-04-30

### Added

- New function `uef_bin:reverse_utf8/1` which returns a binary in reverse character order.

## [1.1.0] - 2019-04-29

### Added

- New function `uef_bin:reverse/1` which returns a binary in reverse byte order.

## [1.0.0] - 2019-04-29

### Added

- New function `uef_format:format_number/3,4`. Supports custom precision, number of decimal digits, thousands separator, decimal point, currency symbols and custom currency position (left/right).

- New function `uef_format:format_price/3` based on `uef_format:format_number/3`.

### Changed

- Module `uef_format` totally reworked.
- Function `uef_format:format_price/1,2` now returns other value, see README.

## [0.1.0] - 2019-04-24

- Intial release
