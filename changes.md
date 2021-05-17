# Changelog

## 0.2.11 (2021-05-17)

- Support testing inside checking and make checking options optional
  ([#65](https://github.com/gfredericks/test.chuck/pull/65))

## 0.2.10 (2019-08-10)

Updates to `gen-string-from-regex`

- It now targets the syntax of java 8 and 11 specifically
- Regex features new in Java 11:
  - `\X` -- this is now parsed but not supported
  - `\N{LATIN CAPITAL LETTER X}` is parsed and supported
  - Large (> UTF-16) code-points in `\N{...}` and in `\x`
    and `\u` expressions are now fully supported
- Parses the absurd edge case `#"\c\Q0"` correctly

## 0.2.9 (2018-04-21)

Adds `com.gfredericks.test.chuck.generators/bounded-recursive-gen`.

## 0.2.8 (2017-07-25)

Upgrades dependency on cljs-time.

## 0.2.7

`com.gfredericks.test.chuck.clojure-test/checking` now can accept a
map of options in place of the number of tests
([#49](https://github.com/gfredericks/test.chuck/pull/49)).

## 0.2.6

Print failing args better in `com.gfredericks.test.chuck.clojure-test/checking`.

## 0.2.5

Stop using deprecated functions internally
([#42](https://github.com/gfredericks/test.chuck/pull/42)) — fixes
compiler warnings in clojurescript.

## 0.2.4

Support non-capturing groups in `string-from-regex`,
e.g. `#"foo(?:bar)*"`.

## 0.2.3

Bugfix: allow empty binding vector in
`com.gfredericks.test.chuck.generators/for` and
`com.gfredericks.test.chuck.properties/for-all` (see
[Issue #40](https://github.com/gfredericks/test.chuck/issues/40)).

## 0.2.2

Various bugfixes and enhancements to
`com.gfredericks.test.chuck.clojure-test`.

`com.gfredericks.test.chuck/times` is now supported in cljs, but
doesn't do anything currently.

## 0.2.1

Changed `com.gfredericks.test.chuck.clojure-test/checking` to use
`com.gfredericks.test.chuck.properties/for-all` instead of
`clojure.test.check.properties/for-all`. This is a mild breaking
change that seems unlikely to affect many users.

## 0.2.0

Mostly support ClojureScript (lacking most notably
`string-from-regex`).

## 0.1.22

Fixed reporting problems in
`com.gfredericks.test.chuck.clojure-test/checking`, see
[this issue](https://github.com/gfredericks/test.chuck/issues/17) for
details.

## 0.1.21

Added `sub-map` generator.

## 0.1.20

Fix bug in `bounded-int`.

## 0.1.19

Added `com.gfredericks.test.chuck.clojure-test/for-all`.

## 0.1.18

Removed reflection.

## 0.1.17

Upgraded instaparse lib for compatibility with clojure 1.7.

## 0.1.16

Bugfix for `com.gfredericks.test.chuck.clojure-test` which was broken
when using `test.check` version `0.7.0`.

## 0.1.15

Even more bugfixes for
`com.gfredericks.test.chuck.properties/for-all`, issue #7 this time
(wherewithrespectintowhich destructuring was completely broken).

## 0.1.14

Fix issue #5, making `com.gfredericks.test.chuck.properties/for-all`
report failing args less uselessly.

## 0.1.13

Added `com.gfredericks.test.chuck.clojure-test`.

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
