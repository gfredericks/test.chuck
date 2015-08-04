(ns com.gfredericks.test.chuck.generators
  "Yes this namespace's name has five components."
  (:refer-clojure :exclude [double for partition])
  #?(:cljs (:require-macros [com.gfredericks.test.chuck.generators :refer [for]]))
  (:require [clojure.test.check.generators :as gen]
            [#?(:clj clojure.core :cljs cljs.core) :as core]
            [#?(:clj clj-time.core :cljs cljs-time.core) :as ct]
            [#?(:clj clj-time.coerce :cljs cljs-time.coerce) :as ctc]
            #?(:clj [com.gfredericks.test.chuck.regexes :as regexes])))

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
          (let [max-tries-meta (-> v2 meta :max-tries)
                max-tries-arg (if max-tries-meta
                                [max-tries-meta])
                v1' `(gen/such-that (fn [~k1] ~v2) ~v1 ~@max-tries-arg)]
            `(for [~k1 ~v1' ~@even-more] ~expr))

          ((some-fn symbol? vector? map? #{:parallel}) k2)
          `(gen/bind ~v1 (fn [~k1] (for ~more ~expr)))

          :else
          (throw (ex-info "Unsupported binding form in gen/for!" {:form k2})))))

(defn subsequence
  "Given a collection, generates \"subsequences\" which are sequences
  of (not necessarily contiguous) elements from the original
  collection, in the same order. For collections of distinct elements
  this is effectively a subset generator, with an ordering guarantee."
  [elements]
  (for [bools (apply gen/tuple (repeat (count elements) gen/boolean))]
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

(defn ^:private scalb
  [x exp]
  #?(:clj  (Math/scalb x exp)
     :cljs (* x (.pow js/Math 2 exp))))

(def double
  "Generates a Double, which can include Infinity and -Infinity
  but not NaN."
  (gen/fmap
   (fn [[signed-significand exp]]
     (scalb (core/double signed-significand) (core/int exp)))
   (gen/tuple
    (let [bignumber (apply * (repeat 52 2))]
      (bounded-int (- bignumber) bignumber))
    (bounded-int -1022 1023))))

#?(:clj
(defn string-from-regex
  "Given a regular expression, returns a generator that generates
  strings matching that regular expression.

  As jvm regular expressions are quite complex, and certain features
  are quite challenging to implement as generators, this function does
  not support all of their features. However, it tries to at least
  accurately recognize features that it doesn't support and throw
  helpful exceptions if it is called with a regular expression using
  any of those features."
  [regex]
  (regexes/gen-string-from-regex regex)))

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

(def valid-offset-fns [ct/millis ct/seconds ct/minutes ct/hours ct/days ct/months ct/years])

(def ^:private valid-offset-fn? (set valid-offset-fns))

(def ^:private yr-2000 (ct/date-time 2000))

(defn datetime
  "Generates datetime within given range and format.

   base-datetime => By default it'll calculate the dates from year 2000.
                    Generally this is a good idea instead of using (ct/now)
                    since giving the same seed will generate the same output.
                    If you would like to generate from a differnt base-datetime,
                    Pass for example (ct/now) to use current time,
                    Or pass a specific date-time (ct/date-time 2011 11 24)

   offset-min & offset-max => The offset number range
                              By default it is -1000 to 1000

   offset-fns => List of functions which will be used with the given offset.
                 It randomly picks one of the functions and
                 applies the random offset with the given range.
                 Check valid-offset-fns for possible values.
                 By default its all the values of valid-offset-fns.

   For example If you would like to generate datetime
   from last 10 months to next 10 months:
   (gen/sample (datetime {:offset-fns [clj-time.core/months]
                          :offset-min -10
                          :offset-max 10}))
   =>
   (#<DateTime 1999-11-01T00:00:00.000Z>
    #<DateTime 1999-12-01T00:00:00.000Z>
    #<DateTime 2000-05-01T00:00:00.000Z>
    ....)"
  ([]
   (datetime {}))
  ([{:keys [base-datetime offset-fns offset-min offset-max]
     :or {offset-fns valid-offset-fns
          offset-min -1000
          offset-max 1000
          base-datetime yr-2000}}]
   {:pre [(<= offset-min offset-max)
          (not-empty offset-fns)
          (every? valid-offset-fn?
                  offset-fns)]}
   (gen/fmap (fn [[offset-fn offset]]
               (->> offset
                   offset-fn
                   (ct/plus base-datetime)))
             (gen/tuple (gen/elements offset-fns)
                        (bounded-int offset-min
                                     offset-max)))))
