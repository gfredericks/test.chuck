(ns com.gfredericks.test.chuck.exception-handling-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as test :refer [test-var test-vars *current-env*]
                      :refer-macros [is testing deftest]])
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test #?(:clj :refer :cljs :refer-macros) [checking]]))

(deftest this-test-should-crash-and-be-caught
  (checking "an int is zero or one" 100 [i gen/int]
    ;; going for uncaught-error-not-in-assertion here
    (case i ; will throw when not in #{0 1}
      0 :zero
      1 :one)))

(defn capture-test-var [v]
  (with-out-str (test-var v)))

(defn capture-report-counters-and-out [test]
  #?(:clj
     (binding [; need to keep the failure of the test
               ; from affecting the clojure.test.check test run
               *report-counters* (ref *initial-report-counters*)
               *test-out* (java.io.StringWriter.)]
       (capture-test-var test)
       [@*report-counters* (str *test-out*)])

     :cljs
     (binding [*current-env* (test/empty-env)]
       (let [out (capture-test-var test)]
         [(:report-counters *current-env*) out]))))

(deftest exception-detection-test
  (let [[test-results out]
        (capture-report-counters-and-out #'this-test-should-crash-and-be-caught)]
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
        (capture-report-counters-and-out #'this-test-should-fail)]
    (is (not (re-find #"not-falsey-or-exception" out)))
    (is (= {:pass 1 ; TODO: why 1? Not sure, but that's what's being reported
            :fail 1
            :error 0}
           (select-keys test-results [:pass :fail :error])))))

(defn test-ns-hook []
  (test-vars [#'exception-detection-test
              #'failure-detection-test]))
