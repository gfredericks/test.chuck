(ns com.gfredericks.test.chuck.clojure-test
  (:require [clojure.test.check :as tc]
            [clojure.test :refer :all]
            [clojure.test.check.properties :as prop]))

(defn report-when-failing [result]
  (is (:result result) result))

(defmacro capture-reports [body]
  `(let [reports# (atom [])]
     (with-bindings [report #(swap! reports# conj %)]
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

(defmacro checking
  "A macro intended to replace the testing macro in clojure.test with a
  generative form. To make (testing \"doubling\" (is (= (* 2 2) (+ 2 2))))
  generative, you simply have to change it to
  (checking \"doubling\" 100 [x gen/int] (is (= (* 2 x) (+ x x)))).

  For more details on this code, see http://blog.colinwilliams.name/blog/2015/01/26/alternative-clojure-dot-test-integration-with-test-dot-check/"
  [name tests bindings & body]
  `(testing ~name
     (let [final-reports# (agent [])]
       (report-when-failing (tc/quick-check ~tests
                              (prop/for-all ~bindings
                                (let [reports# (capture-reports ~body)]
                                  (send final-reports# save-to-final-reports reports#)
                                  (pass? reports#)))))
       (await final-reports#)
       (doseq [r# @final-reports#]
         (report r#)))))
