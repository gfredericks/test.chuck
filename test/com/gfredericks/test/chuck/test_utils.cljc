(ns com.gfredericks.test.chuck.test-utils
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as test :refer [test-var *current-env*]])))

(defn- capture-test-var [v]
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

