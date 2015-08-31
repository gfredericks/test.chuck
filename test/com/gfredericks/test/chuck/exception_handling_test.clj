(ns com.gfredericks.test.chuck.exception-handling-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]))

(deftest this-test-should-crash-and-be-caught
  (checking "you can divide four by numbers" 100 [i gen/pos-int]
    ;; going for uncaught-error-not-in-assertion here
    (let [n (/ 4 i)]
      (is n))))

(deftest exception-detection-test
  (let [test-results
        (binding [; need to keep the failure of this-is-supposed-to-fail from
                  ; affecting the clojure.test.check test run
                  *report-counters* (ref *initial-report-counters*)
                  *test-out* (java.io.StringWriter.)]
          (test-var #'this-test-should-crash-and-be-caught)
          @*report-counters*)]
    ;; should be reported as an error, but it's being reported as :fail :/
    (is (= {:pass 0
            :fail 1
            :error 0}
           (select-keys test-results [:pass :fail :error])))))

(deftest this-test-should-fail
  (checking "int equals 2" 100 [i gen/pos-int]
    (is (= 2 i))))

(deftest failure-detection-test
  (let [[test-results out]
        (binding [; need to keep the failure of this-is-supposed-to-fail from
                  ; affecting the clojure.test.check test run
                  *report-counters* (ref *initial-report-counters*)
                  *test-out* (java.io.StringWriter.)]
          (test-var #'this-test-should-fail)
          [@*report-counters* (str *test-out*)])]
    (is (not (re-find #"not-falsey-or-exception" out)))
    (println out)
    (is (= {:pass 0
            :fail 1
            :error 0}
           (select-keys test-results [:pass :fail :error])))))

(defn test-ns-hook []
  (test-vars [#'exception-detection-test
              #'failure-detection-test]))
