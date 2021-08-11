(ns com.gfredericks.test.chuck.cljs-macros
  "Macros needed by com.gfredericks.test.chuck's cljs support.
  Moved here to avoid compiling them during normal Clojure compilation")

(defmacro ^:internal test-check-factor
  "Not for public consumption. Use com.gfredericks.test.chuck/times.

  Returns the multiplier for test-check runs, which can be set at compile-time
  via the TEST_CHECK_FACTOR environment variable."
  []
  (if-some [s (System/getenv "TEST_CHECK_FACTOR")]
    (Double/parseDouble s)
    1))
