(ns com.gfredericks.test.chuck.clojure-test
  (:require [clojure.test.check :as tc]
            [clojure.test :refer :all]
            [clojure.test.check.properties :as prop]))

;; copied from clojure.test.check, which privatized the function in
;; recent versions.
;;
;; I think there might be plans for test.check to abstract this logic
;; into a protocol or something, so I'm not too bothered by the
;; copypasta for now.
(defn ^:private not-falsey-or-exception?
  [value]
  (and value (not (instance? Throwable value))))

(defn report-when-failing [result]
  (is (not-falsey-or-exception? (:result result)) result))

(defmacro capture-reports [body]
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

(defmacro checking
  "A macro intended to replace the testing macro in clojure.test with a
  generative form. To make (testing \"doubling\" (is (= (* 2 2) (+ 2 2))))
  generative, you simply have to change it to
  (checking \"doubling\" 100 [x gen/int] (is (= (* 2 x) (+ x x)))).

  For more details on this code, see http://blog.colinwilliams.name/blog/2015/01/26/alternative-clojure-dot-test-integration-with-test-dot-check/"
  [name tests bindings & body]
  `(testing ~name
     (let [final-reports# (atom [])]
       (report-when-failing (tc/quick-check ~tests
                              (prop/for-all ~bindings
                                (let [reports# (capture-reports ~body)]
                                  (swap! final-reports# save-to-final-reports reports#)
                                  (pass? reports#)))))
       (doseq [r# @final-reports#]
         (report r#)))))
