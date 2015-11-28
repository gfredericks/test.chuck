(ns com.gfredericks.test.chuck.properties-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :as ct
             #?(:clj :refer :cljs :refer-macros) [defspec]]
            [com.gfredericks.test.chuck.properties :as prop'
             #?@(:cljs [:include-macros true])]
            [com.gfredericks.test.chuck.generators :as gen'
             #?@(:cljs [:include-macros true])]
            #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest is]])))

(deftest it-handles-exceptions-correctly
  (is
   (instance? #?(:clj  Throwable
                 :cljs js/Error)
              (:result
               (tc/quick-check 100
                 (prop'/for-all [i gen/int]
                   (case i ; will throw when not in #{0 1}
                     0 :zero
                     1 :one)))))))

(deftest reported-args-test
  (let [p (prop'/for-all [x gen/nat]
            (not (<= 0 x 10)))
        {:keys [fail]} (tc/quick-check 1000 p)]
    (is (= 1 (count fail)))
    (let [[m] fail]
      (is (= ['x] (keys m)))
      (is (<= 0 (get m 'x) 10)))))

(defspec for-all-destructured-args-work-correctly 10
  (prop'/for-all [[a b] (gen/tuple gen/int gen/int)]
    (+ a b)))

(defspec allows-empty-bindings
  (prop'/for-all []
    true))
