(ns com.gfredericks.test.chuck.clojure-test-output-test
  (:require #?(:clj  [clojure.test :as ct :refer [test-vars deftest is]]
               :cljs [cljs.test :as ct :refer [test-vars] :refer-macros [deftest is]])
            #?(:cljs [cljs.reader :refer [read-string]])
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.test-utils :refer [capture-report-counters-and-out]]
            [com.gfredericks.test.chuck.clojure-test :as chuck #?(:clj :refer :cljs :refer-macros) [checking]]))

(deftest a-failing-test
  (checking "all ints lt 5" 100
    [i gen/int]
    (is (< i 5))))

(defmethod ct/report #?(:clj ::chuck/shrunk :cljs [::ct/default ::chuck/shrunk]) [m]
  (println m))

(deftest failure-output-test
  (let [report-ptn #"\{.*:type :com.gfredericks.test.chuck.clojure-test/shrunk.*\}"
        [test-results out] (capture-report-counters-and-out #'a-failing-test)]
    (is (re-find #"expected: \(< i 5\)" out))
    (is (re-find #"actual: \(not \(< \d 5\)" out))
    (let [tc-report (re-find report-ptn out)]
      (is tc-report)
      (when-let [tc-report (and tc-report (read-string tc-report))]
        (is (not (:result tc-report)))
        (is (= [{'i 5}] (get-in tc-report [:shrunk :smallest])))))))

(defn test-ns-hook []
  (test-vars [#'failure-output-test]))
