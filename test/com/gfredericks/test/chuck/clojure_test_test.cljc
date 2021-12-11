(ns com.gfredericks.test.chuck.clojure-test-test
  (:require #?(:clj  [clojure.test :refer :all])
            #?(:cljs [cljs.test :refer-macros [deftest is]])
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck :refer [times]]
            [com.gfredericks.test.chuck.clojure-test #?(:clj :refer :cljs :refer-macros) [checking for-all]]))

(deftest integer-facts
  (checking "positive" (times 100) [i gen/s-pos-int]
    (is (> i 0)))
  (checking "negative" (times 100) [i gen/s-neg-int]
    (is (< i 0))))

(deftest options-test
  ;; empty map or no options are OK, defaults to 100 tests
  (checking "strings are strings" {} [s gen/string-ascii]
    (is (string? s)))
  (checking "strings are strings" [s gen/string-ascii]
    (is (string? s)))
  ;; passes because the number of tests is small
  (checking "small ints" {:num-tests 5} [i gen/s-pos-int]
    (is (< i 10)))
  ;; passes only because of seed
  (checking "specific values" {:num-tests 5 :seed 9012345678} [i gen/int]
    (is (contains? #{-3 -2 -1 0} i)))
  ;; passes because of max-size
  (checking "short strings" {:num-tests (times 100) :max-size 9} [s gen/string-ascii]
    (is (< (count s) 10))))

(deftest counter
  (checking "increasing" (times 100) [i gen/s-pos-int]
    (let [c (atom i)]
      (swap! c inc)
      (is (= @c (inc i)))
      (swap! c inc)
      (is (> @c 0)))))

(deftest for-all-test
  (let [passing-prop (for-all [x gen/s-pos-int]
                       (is (< x (+ x x))))]
    (is (true? (:result (quick-check 20 passing-prop)))))
  (let [failing-prop (for-all [x gen/s-pos-int]
                       (is true)
                       ;; sticking a failing assertion in between two
                       ;; passing ones
                       (is (zero? x))
                       (is (= x x)))
        ret (quick-check 20 failing-prop)]

    #?(:clj (is (pos? (:fail @*report-counters*))))

    ;; Reset the report counters back because we want the test to pass if the "test" failed
    #?(:clj (dosync (ref-set *report-counters* *initial-report-counters*))
       :cljs (cljs.test/set-env! (cljs.test/empty-env)))

    (is (not (:result ret)))))
