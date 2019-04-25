# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- New function `uef_format:format_number/2,3`. Supports custom precision, trailing zeroes, thousands separator, decimal point, currency symbols, currency position (left, right) and can return a value in custom Erlang type (`binary()` or `string()`).

- New function `uef_format:format_price/3` based on `uef_format:format_number/3`.

### Changed

- Module `uef_format` totally reworked.
- Function `uef_format:format_price/1,2` now returns other value, see README.

## [0.1.0] - 2019-04-24

- Intial release
