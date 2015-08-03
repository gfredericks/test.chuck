(ns com.gfredericks.test.chuck.generators
  "Yes this namespace's name has five components."
  (:require-macros [com.gfredericks.test.chuck.generators :as gen-macros])
  (:refer-clojure :exclude [double for partition])
  (:require [cljs.core :as core]
            [cljs.test.check.generators :as gen]))

(defn subsequence
  "Given a collection, generates \"subsequences\" which are sequences
  of (not necessarily contiguous) elements from the original
  collection, in the same order. For collections of distinct elements
  this is effectively a subset generator, with an ordering guarantee."
  [elements]
  (gen-macros/for [bools (apply gen/tuple (repeat (count elements) gen/boolean))]
    (->> (map list bools elements)
         (filter first)
         (map second))))

(defn subset
  "Deprecated variant of subsequence that coerces the result to a set."
  [elements]
  (gen/fmap set (subsequence elements)))

(defn cap-size
  "Wraps the given generator so that it is never called with a size
  larger than the max given."
  [max-size gen]
  (gen/sized (fn [size]
               (gen/resize (min size max-size) gen))))

(defn partition
  "Generates a collection of collection of the elements in coll, such
  that concatting them together gives the original collection. None of
  the subcollections will be empty."
  ([coll] (partition coll 4))
  ([coll avg-size]
     {:pre [(> avg-size 1)]}
     (if (empty? coll)
       (gen/return [])
       (gen-macros/for [bools (apply gen/tuple (repeat (dec (count coll))
                                            (gen/frequency
                                             [[(dec avg-size) (gen/return false)]
                                              [1 (gen/return true)]])))]
         (reduce (fn [ret [bool x]]
                   (if bool
                     (conj ret [x])
                     (update-in ret [(dec (count ret))] conj x)))
                 [[(first coll)]]
                 (map vector bools (rest coll)))))))

(defn map->hash-map
  "Like test.check.generators/hash-map, but takes a single map argument
  instead of varargs."
  [m]
  (apply gen/hash-map (apply concat m)))


;;
;; Numbers!
;;

(defn bounded-int
  "Like clojure.test.check.generators/choose, but generates
  smallish numbers for small sizes.

  Both bounds are inclusive."
  [low high]
  (gen/sized (fn [size]
               (let [exp (apply * (repeat size 2N))
                     -high-low (- high low)
                     range-size (min (* 2 exp) -high-low)
                     low' (- exp)
                     high' exp]
                 (cond (<= -high-low range-size)
                       (gen/choose low high)

                       (<= low low' high' high)
                       (gen/choose low' high')

                       (< low' low)
                       (gen/choose low (+ low range-size))

                       (< high high')
                       (gen/choose (- high range-size) high))))))

#_(def double
  "Generates a Double, which can include Infinity and -Infinity
  but not NaN."
  (gen/fmap
   (fn [[signed-significand exp]]
     (Math/scalb (core/double signed-significand) (core/int exp)))
   (gen/tuple
    (let [bignumber (apply * (repeat 52 2))]
      (bounded-int (- bignumber) bignumber))
    (bounded-int -1022 1023))))

(defn sub-map
  "Given a concrete map, it'll randomly select keys
   from it thus making it a subset of the given map.
   Note: It can return empty maps as well.

   Example:
   (gen/sample (sub-map {:a 1 :b 2 :c 3}))
   => ({} {:b 2} {:b 2, :c 3} {:a 1} ...)"
  [m]
  (gen/fmap (fn [ks]
              (select-keys m ks))
            (subsequence (keys m))))
