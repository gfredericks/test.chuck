(ns com.gfredericks.test.chuck.properties-test
  (:require-macros [com.gfredericks.test.chuck.properties :as prop'])
  (:require [cljs.test :refer-macros [deftest is]]
            [cljs.test.check :as t.c]
            [cljs.test.check.generators :as gen]
            [cljs.test.check.cljs-test :refer-macros [defspec]]
            ))

(deftest it-handles-exceptions-correctly
  (is
   (instance? js/Error
              (:result
               (t.c/quick-check 100 (prop'/for-all [x gen/int]
                                      (js/Error. "Oops")))))))

(deftest reported-args-test
  (let [p (prop'/for-all [x gen/nat]
            (not (<= 0 x 10)))
        {:keys [fail]} (t.c/quick-check 1000 p)]
    (is (= 1 (count fail)))
    (let [[m] fail]
      (is (= ['x] (keys m)))
      (is (<= 0 (get m 'x) 10)))))

(defspec for-all-destructured-args-work-correctly 10
  (prop'/for-all [[a b] (gen/tuple gen/int gen/int)]
                 (+ a b)))
