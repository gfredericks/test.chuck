(ns com.gfredericks.test.chuck
  #?(:cljs
     (:require-macros [com.gfredericks.test.chuck.cljs-macros :refer [test-check-factor]])))

#?(:clj
   (defn ^:private test-check-factor
     "Returns the multiplier for test-check runs, which can be set at runtime
     via the TEST_CHECK_FACTOR environment variable."
     []
     (if-some [s (System/getenv "TEST_CHECK_FACTOR")]
       (Double/parseDouble s)
       1)))

(defn times
  "A helper function for externally-configurable test run counts.

  Usage:

    (defspec foo-test (times 20)
      ...)

  Simply returns n normally, but if the TEST_CHECK_FACTOR env variable
  is set to a number, n will be multiplied by that number.

  In ClojureScript, TEST_CHECK_FACTOR will retrieved at compile-time."
  [n]
  (long (* n (test-check-factor))))
