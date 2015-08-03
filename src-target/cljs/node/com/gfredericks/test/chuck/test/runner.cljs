(ns com.gfredericks.test.chuck.test.runner
  (:require [cljs.nodejs :as nodejs]
            [cljs.test :as test :refer-macros [run-tests]]
            [com.gfredericks.test.chuck.cljs-test-test]
            [com.gfredericks.test.chuck.properties-test]
            [com.gfredericks.test.chuck.generators-test]))

(nodejs/enable-util-print!)

(defn -main []
  (run-tests
    'com.gfredericks.test.chuck.cljs-test-test
    'com.gfredericks.test.chuck.properties-test
    'com.gfredericks.test.chuck.generators-test))

(set! *main-cli-fn* -main)
