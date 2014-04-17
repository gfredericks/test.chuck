(ns com.gfredericks.test.chuck.generators
  "Yes this namespace's name has five components."
  (:refer-clojure :exclude [for])
  (:require [clojure.test.check.generators :as gen]))

;; Hoping this will be in test.check proper:
;; http://dev.clojure.org/jira/browse/TCHECK-15
(defmacro for
  "Like clojure.core/for, but builds up a generator using bind, fmap,
  and such-that. The right half of each binding pair is a generator,
  and the left half is the value it's generating. The body of the for
  should be a generated value.

  Both :let and :when are available as in  clojure.core/for. Using
  :when will apply a filter to the previous generator via such-that.

  Note that if some of your clauses are independent you should consider
  combining them with tuple so that they can shrink independently.
  E.g., replace
    (for [x g1, y g2] (f x y))
  with
    (for [[x y] (tuple g1 g2)] (f x y))"
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
  ;;   (for [[x y] (fmap (fn [x] (let [y (f x)] [x y])) g)] (h x y))
  ;;
  ;; A :when clause gets absorbed into the preceding clause
  ;; via a transformation with such-that:
  ;;
  ;;   (for [x g, :when (f x)] (h x))
  ;;
  ;; becomes
  ;;
  ;;   (for [x (such-that (fn [x] (f x)) g)] (h x))
  (let [[k1 v1 & [k2 v2 & even-more :as more]] bindings]
    (assert (not (keyword? k1)))
    (cond (empty? more)
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
                       xs (partition 2 v2)]
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
                v1' `(gen/fmap (fn [~k1] (let [~@lettings] [~k1 ~@values])) ~v1)]
            `(for [~k1' ~v1' ~@even-more] ~expr))

          (= k2 :when)
          (let [v1' `(gen/such-that (fn [~k1] ~v2) ~v1)]
            `(for [~k1 ~v1' ~@even-more] ~expr))

          ((some-fn symbol? vector? map?) k2)
          `(gen/bind ~v1 (fn [~k1] (for ~more ~expr)))

          :else
          (throw (ex-info "Unsupported binding form in gen/for!" {:form k2})))))
