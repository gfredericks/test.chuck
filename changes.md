# Changelog

## 0.1.9

Added `string-from-regex` support for Java 8 features (active only
when actually running with Java 8). Thanks again to
[Steve Miner](https://github.com/miner).

## 0.1.8

Fix dependency declaration -- instaparse is a runtime dependency
(thanks [Steve Miner](https://github.com/miner)).

## 0.1.7

Fixed an unsupported-feature-reporting bug in `string-from-regex`.

## 0.1.6

- Added `string-from-regex` generator
- Added optional `^:max-tries` metadata to `:when` clauses in `for` macro
