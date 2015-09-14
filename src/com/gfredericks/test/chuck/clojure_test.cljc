(ns com.gfredericks.test.chuck.clojure-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop
             #?@(:cljs [:include-macros true])]
            #?(:clj  [clojure.test :as ct :refer [is testing]]
               :cljs [cljs.test :as ct :refer-macros [is testing]])))

(defn ^:private not-exception?
  [value]
  (not (instance? #?(:clj  Throwable
                     :cljs js/Error)
                  value)))

(defn report-exception [result]
  (is (not-exception? (:result result)) result))

(defn pass? [reports]
  (every? #(= (:type %) :pass) reports))

(defn report-needed? [reports final-reports]
  (or (not (pass? reports)) (empty? final-reports)))

(defn save-to-final-reports [final-reports reports]
  (if (report-needed? reports final-reports)
    reports
    final-reports))

(def ^:dynamic *chuck-captured-reports*)

#?(:cljs
(defmethod ct/report [::chuck-capture :fail]
  [m]
  (swap! *chuck-captured-reports* conj m)))

#?(:cljs
(defmethod ct/report [::chuck-capture :pass]
  [m]
  (swap! *chuck-captured-reports* conj m)))

(defn capture-reports*
  [reports-atom f]
  #?(:clj
     (binding [ct/report #(swap! reports-atom conj %)]
       (f))

     :cljs
     (binding [*chuck-captured-reports* reports-atom
               cljs.test/*current-env* (cljs.test/empty-env ::chuck-capture)]
       (f))))

(defmacro capture-reports
  [& body]
  `(let [reports# (atom [])]
     (capture-reports* reports# (fn [] ~@body))
     @reports#))

(defmacro qc-and-report-exception
  [final-reports tests bindings & body]
  `(report-exception
    (tc/quick-check
      ~tests
      (prop/for-all ~bindings
        (let [reports# (capture-reports ~@body)]
          (swap! ~final-reports save-to-final-reports reports#)
          (pass? reports#))))))

(defn -testing
  [name func]
  (testing name (func)))

(defn -report
  [reports]
  (ct/report reports))

(defmacro checking
  "A macro intended to replace the testing macro in clojure.test with a
  generative form. To make (testing \"doubling\" (is (= (* 2 2) (+ 2 2))))
  generative, you simply have to change it to
  (checking \"doubling\" 100 [x gen/int] (is (= (* 2 x) (+ x x)))).

  For more details on this code, see http://blog.colinwilliams.name/blog/2015/01/26/alternative-clojure-dot-test-integration-with-test-dot-check/"
  [name tests bindings & body]
  `(-testing ~name
    (fn []
      (let [final-reports# (atom [])]
        (qc-and-report-exception final-reports# ~tests ~bindings ~@body)
        (doseq [r# @final-reports#]
          (-report r#))))))

(defmacro for-all
  "An alternative to clojure.test.check.properties/for-all that uses
  clojure.test-style assertions (i.e., clojure.test/is) rather than
  the truthiness of the body expression."
  [bindings & body]
  `(prop/for-all
     ~bindings
     (let [reports# (capture-reports ~@body)]
       (pass? reports#))))
