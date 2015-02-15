(ns com.gfredericks.test.chuck.properties
  "Alternative to clojure.test.check.properties."
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.generators :as gen']))

(declare for-bindings)

;;
;; At some point it would be cleaner to separate bindings used
;; directly with generators from bindings used with :let, and hide the
;; latter (in metadata?) so it doesn't come up as an arg to the
;; property
;;

(defn ^:private for-bindings-in-binding-expr
  [expr]
  ;; this'll get gensyms o_O
  (->> (clojure.core/destructure [expr :dummy])
       (partition 2)
       (map first)))

(defn ^:private for-bindings-in-clause
  [left right]
  (cond (= :let left) (->> right
                           (partition 2)
                           (map first)
                           (mapcat for-bindings-in-binding-expr))
        (= :when left) []
        (= :parallel left) (for-bindings right)
        (or (symbol? left) (map? left) (vector? left)) (for-bindings-in-binding-expr left)
        :else (throw (ex-info "Unrecognized binding expression in test.chuck.properties/for-all!"
                              {:expr left}))))

(defn ^:private for-bindings
  [clauses]
  (->> (partition 2 clauses)
       (mapcat (fn [[left right]] (for-bindings-in-clause left right)))
       (distinct)))

(defmacro for-all
  "Alternative version of clojure.test.check.properties/for-all where
  the binding forms are interpreted as per
  com.gfredericks.test.chuck.generators/for."
  [bindings expr]
  (let [bound-names (for-bindings bindings)
        quoted-names (map #(list 'quote %) bound-names)]
    `(prop/for-all [{:syms [~@bound-names]}
                    (gen'/for ~bindings
                      (with-meta
                        ~(zipmap quoted-names bound-names)
                        {::for-all-bindings-map true}))]
       ~expr)))
