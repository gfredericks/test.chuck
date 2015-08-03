(ns com.gfredericks.test.chuck.cljs-test-test
  (:require [cljs.test :refer :all]
            [cljs.test.check :refer [quick-check]]
            [cljs.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer :all]))

(deftest integer-facts
  (checking "positive" 100 [i gen/s-pos-int]
    (is (> i 0)))
  (checking "negative" 100 [i gen/s-neg-int]
    (is (< i 0))))

(deftest counter
  (checking "increasing" 100 [i gen/s-pos-int]
    (let [c (atom i)]
      (swap! c inc)
      (is (= @c (inc i)))
      (swap! c inc)
      (is (> @c 0)))))

(deftest exception-detection-test
  (eval '(do (ns fake.test.namespace
               (:require [clojure.test :refer :all]
                         [clojure.test.check.generators :as gen]
                         [com.gfredericks.test.chuck.clojure-test :refer :all]))
             (deftest this-test-should-crash
               (checking "you can divide four by numbers" 100 [i gen/pos-int]
                 ;; going for uncaught-error-not-in-assertion here
                 (let [n (/ 4 i)]
                   (is n))))))
  (let [test-results
        (binding [clojure.test/*test-out* (java.io.StringWriter.)]
          (clojure.test/run-tests (the-ns 'fake.test.namespace)))]
    ;; should this be reported as an error for sure?
    (is (= 1 (+ (:error test-results)
                (:fail test-results)))))
  (remove-ns 'fake.test.namespace))

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
