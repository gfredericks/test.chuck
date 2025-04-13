(ns com.gfredericks.test.chuck.clojure-test-output-test
  (:require [clojure.test :as ct :refer [test-vars deftest is testing]]
            #?(:cljs [cljs.reader :refer [read-string]])
            [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.test-utils :refer [capture-report-counters-and-out]]
            [com.gfredericks.test.chuck.clojure-test :as chuck :refer [checking]]))

(deftest a-failing-test
  (checking "all ints lt 5" 100
    [i gen/int]
    (testing "test `testing` logging1"
      (is (< i 5) "test `is` logging1"))
    (testing "test `testing` logging2"
      (is (< i 5 6) "test `is` logging2"))))

(deftest an-error-test
  (checking "error case" {:num-tests 1
                          :seed 1634575704028}
    [i gen/int]
    (testing "an error"
      (throw (ex-info "thrown exception"
                      ;; things that require `pr-str` for round-trip
                      {:foo nil
                       :bar "baz"})))))

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

(deftest error-output-test
  (let [[test-results all-out] (capture-report-counters-and-out #'an-error-test)
        ;; skip test preamble
        [_ out] (str/split all-out
                           #".*error case\n"
                           2)
        _ (assert out (pr-str all-out))
        ;; report is printed after `testing`
        tc-report (edn/read-string
                    {:readers (assoc default-data-readers
                                     'error #(-> %
                                                 (assoc ::error-tag true))
                                     ;; Babashka outputs #object tags for namespaces, so we need to handle them
                                     #?@(:bb ['object (constantly nil)]))}
                    out)
        error-map-msg-key #?(:clj :cause :cljs :message)]
    (testing "clojure.test reporting"
      (is (= test-results {:test 1, :pass 0, :fail 1, :error 0})))
    (testing "thrown exception"
      (is (false? (:pass? tc-report))
          (pr-str tc-report))
      (doseq [path [[:result]
                    [:result-data :clojure.test.check.properties/error]
                    [:shrunk :result]
                    [:shrunk :result-data :clojure.test.check.properties/error]]]
        (testing path
          (is (= {::error-tag true
                  error-map-msg-key "thrown exception"
                  :data {:foo nil, :bar "baz"}}
                 (-> (get-in tc-report path)
                     (select-keys #{::error-tag error-map-msg-key :data})))
              (get-in tc-report path))))
      (is (= [{'i 0}] (get-in tc-report [:shrunk :smallest]))))))

(defn test-ns-hook []
  (test-vars [#'failure-output-test
              #'error-output-test]))
