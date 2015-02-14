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

(deftest reported-args-test
  (let [p (prop'/for-all [x gen/nat]
            (not (<= 0 x 10)))
        {:keys [fail]} (t.c/quick-check 1000 p)]
    (is (= 1 (count fail)))
    (let [[m] fail]
      (is (= ['x] (keys m)))
      (is (<= 0 (get m 'x) 10)))))
