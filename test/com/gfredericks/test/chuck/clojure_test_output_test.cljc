(ns com.gfredericks.test.chuck.clojure-test-output-test
  (:require #?(:clj  [clojure.test :as ct :refer [test-vars deftest is testing]]
               :cljs [cljs.test :as ct :refer [test-vars] :refer-macros [deftest is testing]])
            #?(:cljs [cljs.reader :refer [read-string]])
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.test-utils :refer [capture-report-counters-and-out]]
            [com.gfredericks.test.chuck.clojure-test :as chuck #?(:clj :refer :cljs :refer-macros) [checking]]))

(deftest a-failing-test
  (checking "all ints lt 5" 100
    [i gen/int]
    (testing "test `testing` logging1"
      (is (< i 5) "test `is` logging1"))
    (testing "test `testing` logging2"
      (is (< i 5 6) "test `is` logging2"))))

(defmethod ct/report #?(:clj ::chuck/shrunk :cljs [::ct/default ::chuck/shrunk]) [m]
  (println m))

(deftest failure-output-test
  (let [[test-results out] (capture-report-counters-and-out #'a-failing-test)
        ;; shrunk map should be printed first
        tc-report (read-string out)
        ;; clearly distinguish between actual test returns and simulated ones
        msg (with-out-str (pp/pprint (str/split-lines out)))]
    (testing "clojure.test reporting"
      (is (= test-results {:test 1, :pass 0, :fail 2, :error 0}))
      (is (str/includes? 
            out
            (str/join
              \newline
              ["all ints lt 5 test `testing` logging1"
               "test `is` logging1"
               "expected: (< i 5)"
               "  actual: (not (< 5 5))"]))
          msg)
      (is (str/includes?
            out 
            (str/join
              \newline
              ["all ints lt 5 test `testing` logging2"
               "test `is` logging2"
               "expected: (< i 5 6)"
               "  actual: (not (< 5 5 6))"]))
          msg))
    (testing "test.check reporting"
      (is (map? tc-report))
      (is (= :com.gfredericks.test.chuck.clojure-test/shrunk (:type tc-report)))
      (is (not (:result tc-report)))
      (is (= [{'i 5}] (get-in tc-report [:shrunk :smallest]))))))

(defn test-ns-hook []
  (test-vars [#'failure-output-test]))
