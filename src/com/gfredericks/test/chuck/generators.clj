(ns com.gfredericks.test.chuck.generators
  "Yes this namespace's name has five components."
  (:refer-clojure :exclude [double for partition])
  (:require [clojure.core :as core]
            [clojure.test.check.generators :as gen]))

;; Hoping this will be in test.check proper:
;; http://dev.clojure.org/jira/browse/TCHECK-15
(defmacro for
  "Like clojure.core/for, but builds up a generator using bind, fmap,
  and such-that. The right half of each binding pair is a generator,
  and the left half is the value it's generating. The body of the for
  should be a generated value.

  Both :let and :when are available as in clojure.core/for. Using
  :when will apply a filter to the previous generator via such-that.

  An additional available clause is the :parallel clause, which is an
  alternative to tuple, for use when several generators are
  independent."
  [bindings expr]
  ;; The strategy here is to rewrite the expression one clause at
  ;; a time using two varieties of recursion:
  ;;
  ;; A basic single-clause form expands to fmap:
  ;;
  ;;   (for [x g] (f x))
  ;;
  ;; becomes
  ;;
  ;;   (fmap (fn [x] (f x)) g)
  ;;
  ;; Multiple clauses expand one at a time to a call to bind with
  ;; a nested for expression:
  ;;
  ;;   (for [x1 g1, x2 g2] (f x1 x2))
  ;;
  ;; becomes
  ;;
  ;;   (bind g1 (fn [x1] (for [x2 g2] (f x1 x2))))
  ;;
  ;; A :let clause gets absorbed into the preceding clause via
  ;; a transformation with fmap and tuple destructuring:
  ;;
  ;;   (for [x g, :let [y (f x)]] (h x y))
  ;;
  ;; becomes
  ;;
  ;;   (for [[x y] (fmap (fn [arg]
  ;;                       (let [x arg, y (f x)]
  ;;                         [arg y]))
  ;;                     g)]
  ;;     (h x y))
  ;;
  ;; A :when clause gets absorbed into the preceding clause
  ;; via a transformation with such-that:
  ;;
  ;;   (for [x g, :when (f x)] (h x))
  ;;
  ;; becomes
  ;;
  ;;   (for [x (such-that (fn [x] (f x)) g)] (h x))
  ;;
  ;; A :parallel clause is easily transformed to a call to
  ;; gen/tuple:
  ;;
  ;;   (for [:parallel [v1 g1, v2 g2]] (f v1 v2))
  ;;
  ;; becomes
  ;;
  ;;   (for [[v1 v2] (gen/tuple g1 g2)] (f v1 v2))
  (let [[k1 v1 & [k2 v2 & even-more :as more]] bindings]
    (assert (or (= :parallel k1) (not (keyword? k1))))
    (cond (= :parallel k1)
          (do (assert (even? (count v1))
                      ":parallel clause must have an even number of bindings!")
              (let [pairs (core/partition 2 v1)
                    names (map first pairs)
                    gens (map second pairs)]
                `(for [[~@names] (gen/tuple ~@gens)
                       ~@more]
                   ~expr)))

          (empty? more)
          ;; special case to avoid extra call to fmap
          (if (and (symbol? k1) (= k1 expr))
            v1
            `(gen/fmap (fn [~k1] ~expr) ~v1))

          (= k2 :let)
          ;; This part is complex because we need to watch out for
          ;; destructuring inside the :let, since the destructuring
          ;; form can't be used as a value expression.
          ;;
          ;; This loop is constructing three collections:
          ;;
          ;;   lettings - The kv pairs for the let inside the fmap fn
          ;;   bindings - The single tuple-destructuring form used
          ;;              in the outer for expression
          ;;   values   - The value expressions that go in the vector
          ;;              that is the return value from the fmap fn
          (let [[lettings bindings values]
                (loop [lettings []
                       bindings []
                       values   []
                       xs (core/partition 2 v2)]
                  (if-let [[[k v] & xs] (seq xs)]
                    (if (symbol? k)
                      (recur (conj lettings k v)
                             (conj bindings k)
                             (conj values k)
                             xs)
                      (let [k' (gensym)]
                        (recur (conj lettings k' v k k')
                               (conj bindings k)
                               (conj values k')
                               xs)))
                    [lettings bindings values]))
                k1' (apply vector k1 bindings)
                v1' `(gen/fmap (fn [arg#]
                                 (let [~k1 arg#
                                       ~@lettings]
                                   [arg# ~@values]))
                               ~v1)]
            `(for [~k1' ~v1' ~@even-more] ~expr))

          (= k2 :when)
          (let [v1' `(gen/such-that (fn [~k1] ~v2) ~v1)]
            `(for [~k1 ~v1' ~@even-more] ~expr))

          ((some-fn symbol? vector? map? #{:parallel}) k2)
          `(gen/bind ~v1 (fn [~k1] (for ~more ~expr)))

          :else
          (throw (ex-info "Unsupported binding form in gen/for!" {:form k2})))))

(defn subset
  "Generates a subset of the given elements, which may be
  empty or include all of the elements."
  [elements]
  (for [bools (apply gen/tuple (repeat (count elements) gen/boolean))]
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
  ([coll] (partition coll 4))
  ([coll avg-size]
     {:pre [(> avg-size 1)]}
     (if (empty? coll)
       (gen/return [])
       (for [bools (apply gen/tuple (repeat (dec (count coll))
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
  [min max]
  (gen/sized (fn [size]
               (let [exp (apply * (repeat size 2N))
                     min' (clojure.core/max min (- exp))
                     max' (clojure.core/min max exp)]
                 (gen/choose min' max')))))

(def double
  "Generates a Double, which can include Infinity and -Infinity
  but not NaN."
  (gen/fmap
   (fn [[signed-significand exp]]
     (Math/scalb (core/double signed-significand) exp))
   (gen/tuple
    (let [bignumber (apply * (repeat 52 2))]
      (bounded-int (- bignumber) bignumber))
    (bounded-int -1022 1023))))
