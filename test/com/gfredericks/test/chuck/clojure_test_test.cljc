(ns com.gfredericks.test.chuck.clojure-test-test
  (:require #?(:clj  [clojure.test :refer :all])
            #?(:cljs [cljs.test :refer-macros [deftest is testing]])
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test #?(:clj :refer :cljs :refer-macros) [checking for-all]]))

(deftest integer-facts
  (checking "positive" 100 [i gen/s-pos-int]
    (is (> i 0)))
  (checking "negative" 100 [i gen/s-neg-int]
    (is (< i 0))))

(deftest options-test
  ;; no option is OK, defaults to 100 tests
  (let [nb-runs (atom 0)]
    (checking "no option works" [i gen/s-pos-int]
      (swap! nb-runs inc)
      (is (pos? i)))
    (testing "no option means 100 runs (test.check's default)"
      (is (= 100 @nb-runs))))
  ;; empty map is OK, defaults to 100 tests
  (checking "strings are strings" {} [s gen/string-ascii]
    (is (string? s)))
  ;; passes because the number of tests is small
  (checking "small ints" {:num-tests 5} [i gen/s-pos-int]
    (is (< i 10)))
  ;; passes only because of seed
  (checking "specific values" {:num-tests 5 :seed 12345678} [i gen/int]
    (is (contains? #{-1 0 1} i)))
  ;; passes because of max-size
  (checking "short strings" {:num-tests 100 :max-size 9} [s gen/string-ascii]
    (is (< (count s) 10)))
  ;; bad options throws
  (testing "bad option throws"
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Object)
                 (eval `(checking "numbers are numbers" "opts as string" [i gen/int]
                          (is (int? i))))))))

(deftest counter
  (checking "increasing" 100 [i gen/s-pos-int]
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
                       (is (= x x)))]
    (is (not (:result (quick-check 20 failing-prop))))))
