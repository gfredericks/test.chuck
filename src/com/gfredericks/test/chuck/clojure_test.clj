(ns com.gfredericks.test.chuck.clojure-test
  (:require [clojure.test.check :as tc]
            [clojure.test :refer :all]
            [clojure.test.check.properties :as prop]))

(defmacro capture-reports [& body]
  `(let [reports# (atom [])]
     (binding [report #(swap! reports# conj %)]
       ~@body)
     @reports#))

(defn pass? [reports]
  (every? #(= (:type %) :pass) reports))

(defn report-needed? [reports final-reports]
  (or (not (pass? reports)) (empty? final-reports)))

(defn save-to-final-reports [final-reports reports]
  (if (report-needed? reports final-reports)
    reports
    final-reports))

(defn not-exception?
  [value]
  (not (instance? Throwable value)))

(defn report-exception [result]
  (is (not-exception? (:result result)) result))

(defmacro checking
  "A macro intended to replace the testing macro in clojure.test with a
  generative form. To make (testing \"doubling\" (is (= (* 2 2) (+ 2 2))))
  generative, you simply have to change it to
  (checking \"doubling\" 100 [x gen/int] (is (= (* 2 x) (+ x x)))).

  For more details on this code, see http://blog.colinwilliams.name/blog/2015/01/26/alternative-clojure-dot-test-integration-with-test-dot-check/"
  [name tests bindings & body]
  (assert (string? name) "name should be a string")
  (assert (vector? bindings) "bindings should be a vector")
  (assert (seq body) "body shouldn't be empty - it means you aren't testing anything")
  `(testing ~name
     (let [final-reports# (atom [])]
       (report-exception (tc/quick-check ~tests
                           (prop/for-all ~bindings
                             (let [reports# (capture-reports ~@body)]
                               (swap! final-reports# save-to-final-reports reports#)
                               (pass? reports#)))))
       (doseq [r# @final-reports#]
         (report r#)))))

(defmacro for-all
  "An alternative to clojure.test.check.properties/for-all that uses
  clojure.test-style assertions (i.e., clojure.test/is) rather than
  the truthiness of the body expression."
  [bindings & body]
  `(prop/for-all ~bindings
     (pass? (capture-reports ~@body))))
