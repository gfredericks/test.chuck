# Changelog

## 0.1.15

Even more bugfixes for
`com.gfredericks.test.chuck.properties/for-all`, issue #7 this time
(wherewithrespectintowhich destructuring was completely broken).

## 0.1.14

Fix issue #5, making `com.gfredericks.test.chuck.properties/for-all`
report failing args less uselessly.

## 0.1.12

Bugfix in `com.gfredericks.test.chuck.properties/for-all`

## 0.1.11

Added `com.gfredericks.test.chuck.properties`

## 0.1.10

Added `subsequence` to replace `subset`, which is deprecated.

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
