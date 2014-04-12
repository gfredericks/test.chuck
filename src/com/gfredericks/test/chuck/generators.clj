(ns com.gfredericks.test.chuck.generators
  (:refer-clojure :exclude [partition])
  (:require [clojure.test.check.generators :as gen]))

(defn subset
  "Generates a subset of the given elements, which may be
  empty or include all of the elements."
  [elements]
  (gen/for [bools (apply gen/tuple (repeat (count elements) gen/boolean))]
    (->> (map list bools elements)
         (filter first)
         (map second)
         (set))))

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
  ([coll] (gen-partition coll 4))
  ([coll avg-size]
     {:pre [(> avg-size 1)]}
     (if (empty? coll)
       (gen/return [])
       (gen/for [bools (apply gen/tuple (repeat (dec (count coll))
                                                (gen/frequency
                                                 [[(dec avg-size) (gen/return false)]
                                                  [1 (gen/return true)]])))]
         (reduce (fn [ret [bool x]]
                   (if bool
                     (conj ret [x])
                     (update-in ret [(dec (count ret))] conj x)))
                 [[(first coll)]]
                 (map vector bools (rest coll)))))))
