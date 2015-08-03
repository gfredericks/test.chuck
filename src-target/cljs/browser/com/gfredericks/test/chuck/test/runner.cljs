(ns com.gfredericks.test.chuck.test.runner
  (:require [cljs.test :as test :refer-macros [run-tests]]
            [com.gfredericks.test.chuck.cljs-test-test]
            [com.gfredericks.test.chuck.properties-test]
            [com.gfredericks.test.chuck.generators-test]))

(enable-console-print!)

(run-tests
    'com.gfredericks.test.chuck.cljs-test-test
    'com.gfredericks.test.chuck.properties-test
    'com.gfredericks.test.chuck.generators-test)
