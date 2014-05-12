(ns com.gfredericks.test.chuck)

(defn ^:private test-check-factor
  "Returns the multiplier for test-check runs, which can be set via
  the TEST_CHECK_FACTOR environment variable."
  []
  (if-let [s (System/getenv "TEST_CHECK_FACTOR")]
    (Double/parseDouble s)
    1))

(defn times
  "A helper function for externally-configurable test run counts.

  Usage:

    (defspec foo-test (times 20)
      ...)

  Simply returns n normally, but if the TEST_CHECK_FACTOR env variable
  is set to a number, n will be multiplied by that number."
  [n]
  (long (* n (test-check-factor))))
