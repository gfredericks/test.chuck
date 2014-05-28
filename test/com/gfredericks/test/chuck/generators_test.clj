(ns com.gfredericks.test.chuck.generators-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.generators :as gen']))

(def lists-and-counts
  (gen'/for [nums (gen/vector gen/nat)
             :let [cardinality (count nums)]]
    [nums cardinality]))

(defspec for-works-correctly 100
  (prop/for-all [[nums cardinality] lists-and-counts]
    (= (count nums) cardinality)))

(def lists-with-two-of-their-elements
  (gen'/for [nums (gen/vector gen/nat)
             :let [cardinality (count nums)]
             :when (> cardinality 1)
             x (gen/elements nums)
             :let [[befores [_x & afters]] (split-with #(not= % x) nums)
                   nums-x (concat befores afters)]
             y (gen/elements nums-x)]
    [nums x y]))

(defspec complex-for-works-correctly 100
  (prop/for-all [[nums x y] lists-with-two-of-their-elements]
    (let [f (frequencies nums)]
      ;; check that both x and y are in the list
      (or (and (= x y) (> (f x) 1))
          (and (not= x y) (pos? (f x)) (pos? (f y)))))))

(def destructuring-usage
  (gen'/for [{:keys [foo]} (gen/hash-map :foo gen/nat)
             :let [unused-binding 42]
             vs (gen/vector gen/boolean foo)]
    [foo vs]))

(defspec destructuring-usage-spec 100
  (prop/for-all [[n vs] destructuring-usage]
    (= n (count vs))))

(def parallel-usage
  (gen'/for [:parallel [x gen/nat
                        y gen/boolean]]
    [x y]))

(defspec parallel-usage-spec 100
  (prop/for-all [[x y] parallel-usage]
    (and (>= x 0)
         (or (= true y) (= false y)))))

(def parallel-as-second-clause
  (gen'/for [n gen/nat
             :parallel [v1 (gen/vector gen/boolean n)
                        v2 (gen/vector gen/boolean n)]]
    [n (concat v1 v2)]))

(defspec parallel-as-second-clause-spec 100
  (prop/for-all [[n v] parallel-as-second-clause]
    (= (* 2 n) (count v))))
