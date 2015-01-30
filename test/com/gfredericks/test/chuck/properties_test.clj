(ns com.gfredericks.test.chuck.properties-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :as t.c]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.properties :as prop']))

(deftest it-handles-exceptions-correctly
  (is
   (instance? Throwable
              (:result
               (t.c/quick-check 100 (prop'/for-all [x gen/int]
                                      (/ 4 0)))))))
