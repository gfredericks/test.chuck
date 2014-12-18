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

## Contributing

I welcome pull requests for any test.check utility that seems halfway
reasonable.

## License

Copyright © 2014 Gary Fredericks

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
