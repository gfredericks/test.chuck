# test.chuck

<img src="http://upload.wikimedia.org/wikipedia/commons/thumb/6/6b/Groundhog2.jpg/320px-Groundhog2.jpg" title="Chuck" align="right" />

_test.chuck_ is a dumping ground of utilities for use with
[test.check](https://github.com/clojure/test.check).

```



       (this space intentionally left blank)



```

## Usage

Leiningen dependency coordinates:

[![version](https://clojars.org/com.gfredericks/test.chuck/latest-version.svg)](https://clojars.org/com.gfredericks/test.chuck)

### General Helpers

``` clojure
(require '[com.gfredericks.test.chuck :as chuck])
```

#### `times`

A helper function for being able to scale your test run count with an
environment variable. To use with `defspec`, simply wrap your test-count
argument in a call to `times`:

``` clojure
(defspec foo-bar-test (chuck/times 20)
  ...)
```

This will normally run the test 20 times, but if you set the
`TEST_CHECK_FACTOR` environment variable to e.g. `3.5`, it will run
the tests 70 times.

### Generators

``` clojure
(require '[com.gfredericks.test.chuck.generators :as gen'])
```

There are a few minor generators and helpers, see the docstrings
for details:

- `for` (described below)
- `string-from-regex`
- `subset`
- `cap-size`
- `partition`
- `map->hash-map`
- `bounded-int`
- `double`

#### `for`

A macro that uses the syntax of `clojure.core/for` to provide the functionality
of `gen/bind`, `gen/fmap`, `gen/such-that`, and `gen/tuple`:

``` clojure
(gen'/for [len gen/nat
           bools (gen/vector gen/boolean len)]
  [len bools])

(gen/sample *1)
;; => ([0 []]
;;     [0 []]
;;     [2 [false true]]
;;     [3 [true false true]]
;;     [1 [true]]
;;     [5 [true true false false true]]
;;     [2 [false false]]
;;     [1 [true]]
;;     [8 [true false false true false false false false]]
;;     [1 [true]])
```

``` clojure
(gen'/for [:parallel [n1 gen/nat
                      n2 gen/nat]
           :when ^{:max-tries 20} (coprime? n1 n2)
           :let [product (* n1 n2)]]
  {:n product, :factors [n1 n2]})

(gen/sample *1)
;; => ({:n 1, :factors [1 1]}
;;     {:n 0, :factors [1 0]}
;;     {:n 2, :factors [1 2]}
;;     {:n 0, :factors [0 1]}
;;     {:n 6, :factors [3 2]}
;;     {:n 20, :factors [4 5]}
;;     {:n 0, :factors [1 0]}
;;     {:n 4, :factors [1 4]}
;;     {:n 24, :factors [3 8]}
;;     {:n 14, :factors [2 7]})
```

#### `string-from-regex`

`string-from-regex` is a suspiciously robust generator that will
generate strings matching a regular expression:

``` clojure
user> (gen/sample (gen'/string-from-regex #"([☃-♥]{3}|B(A|OO)M)*"))
(""
 "☍♛☽"
 ""
 "♂♡☱BAM"
 "♥☩♏BAMBAM"
 ""
 "☓☪☤BAMBAMBOOMBOOM☑☔☟"
 ""
 "BOOM☻☘☌☏☜♋BAM♑♒♛BAMBAM"
 "BOOMBAM♅☧♉☎☐♘BOOM☥♜☐")
```

It does not work with **every** regular expression, but its goal is to
correctly recognize (and report) the usage of unsupported features,
and to handle supported features in a comprehensive way.

##### Shrinking

Generated strings shrink in a natural way:

``` clojure
(def gen-cool-string
  (gen'/string-from-regex
   #"This string has (1 [A-Z]|[2-9]\d* [A-Z]'s)((, (1 [A-Z]|[2-9]\d* [A-Z]'s))*, and (1 [A-Z]|[2-9]\d* [A-Z]'s))?\."))

(def bad-prop
  (prop/for-all [s gen-cool-string]
    (not (re-find #"1 F" s))))

(t.c/quick-check 1000 bad-prop)
=>
{:fail ["This string has 6309694848500700538 H's, 79102649012623413352 F's, 1 F, 59860 U's, 1 T, 1 W, 1 B, and 1 M."],
 :failing-size 26,
 :num-tests 27,
 :result false,
 :seed 1418877588316,
 :shrunk {:depth 8,
          :result false,
          :smallest ["This string has 1 A, 1 F, and 1 A."],
          :total-nodes-visited 27}}
```

##### Unsupported regex features

Some of these could be supported with a bit of effort.

- All flags: `(?i)`, `(?s)`, etc.
- Lookahead and lookbehind
- Reluctant and Possesive quantifiers: `X??`, `X*+`, etc.
  - I'm not sure what these would mean anyhow
- Anchors: `\b`, `$`, `\A`, `$`...
- Backreferences
  - This is tricky at least because it introduces the possibility of
    unmatchable expressions
- Character class [intersections](http://www.regular-expressions.info/charclassintersect.html)
- The hex syntax for unicode characters outside the BMP: `\x{10001}`
- Named character classes: `\p{IsAlphabetic}`, `\P{ASCII}`, ...

## Contributing

I welcome pull request for any test.check utility that seems halfway
reasonable.

## License

Copyright © 2014 Gary Fredericks

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
