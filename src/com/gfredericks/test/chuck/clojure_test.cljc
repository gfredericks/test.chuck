(ns com.gfredericks.test.chuck.clojure-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as tc.clojure-test]
            [com.gfredericks.test.chuck.properties :as prop
             #?@(:cljs [:include-macros true])]
            #?(:clj  [clojure.test :as ct :refer [is testing]]
               :cljs [cljs.test :as ct :refer-macros [is testing]])))

;; exists in clojure.test.check.clojure-test v0.9.0
(defn with-test-out* [f]
  #?(:clj  (ct/with-test-out (f))
     :cljs (f)))

(defn ^:private not-exception?
  [value]
  (not (instance? #?(:clj  Throwable
                     :cljs js/Error)
                  value)))

(defn shrunk-report [m]
  (merge (dissoc m :result) {:type ::shrunk}))

(defmethod ct/report #?(:clj ::shrunk :cljs [::ct/default ::shrunk]) [m]
  (newline)
  (println "Tests failed, smallest case:" (pr-str (-> m :shrunk :smallest))
           "\nSeed" (:seed m)))

(defn report-exception-or-shrunk [result]
  (if (:result result)
    (is (not-exception? (:result result)) result)
    (do
      (with-test-out*
        (fn [] (ct/report (shrunk-report result)))))))

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
(defmethod ct/report [::chuck-capture :error]
  [m]
  (swap! *chuck-captured-reports* conj m)))

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

(defn times [num-tests-or-options]
  (cond (map? num-tests-or-options)     (:num-tests num-tests-or-options tc.clojure-test/*default-test-count*)
        (integer? num-tests-or-options) num-tests-or-options))

(defn options [num-tests-or-options]
  (cond (map? num-tests-or-options)     (dissoc num-tests-or-options :num-tests)
        (integer? num-tests-or-options) {}))

(defmacro qc-and-report-exception
  [final-reports num-tests-or-options bindings & body]
  `(report-exception-or-shrunk
     (let [num-tests-or-options# ~num-tests-or-options]
       (apply tc/quick-check
         (times num-tests-or-options#)
         (prop/for-all ~bindings
           (let [reports# (capture-reports ~@body)]
             (swap! ~final-reports save-to-final-reports reports#)
             (pass? reports#)))
         (apply concat (options num-tests-or-options#))))))

(defn -testing
  [name func]
  (testing name (func)))

(defn -report
  [reports]
  (ct/report reports))

(defmacro checking
  ^{:doc      "A macro intended to replace the testing macro in clojure.test with a
  generative form. To make (testing \"doubling\" (is (= (* 2 2) (+ 2 2))))
  generative, you simply have to change it to
  (checking \"doubling\" 100 [x gen/int] (is (= (* 2 x) (+ x x)))).

  You can optionally pass in a map options instead of the number of tests,
  which will be passed to `clojure.test.check/quick-check`, e.g.:

    (checking \"doubling\" {:num-tests 100 :seed 123 :max-size 10}
      [x gen/int]
      (is (= (* 2 x) (+ x x))))

  For background, see
  http://blog.colinwilliams.name/blog/2015/01/26/alternative-clojure-dot-test-integration-with-test-dot-check/"
    :arglists '([name bindings body] [name num-tests-or-options bindings body])}
  [name & check-decl]
  (let [[num-tests-or-options bindings body] (cond
                                               (and (or number? (first check-decl)
                                                        map? (first check-decl))
                                                    (vector? (second check-decl)))
                                               [(first check-decl) (second check-decl) (nnext check-decl)]

                                               (vector? (first check-decl))
                                               [nil (first check-decl) (next check-decl)]

                                               :else (throw (IllegalArgumentException. "Arguments to `checking` must be either [name bindings body] [name num-tests-or-options bindings body] or [name num-tests-or-options bindings body]")))
        num-tests-or-options (tc.clojure-test/process-options num-tests-or-options)]
    `(-testing ~name
               (fn []
                 (let [final-reports# (atom [])]
                   (qc-and-report-exception final-reports# ~num-tests-or-options ~bindings ~@body)
                   (doseq [r# @final-reports#]
                     (-report r#)))))))

(defmacro for-all
  "An alternative to clojure.test.check.properties/for-all that uses
  clojure.test-style assertions (i.e., clojure.test/is) rather than
  the truthiness of the body expression."
  [bindings & body]
  `(prop/for-all
     ~bindings
     (let [reports# (capture-reports ~@body)]
       (pass? reports#))))
