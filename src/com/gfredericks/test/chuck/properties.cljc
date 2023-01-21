(ns com.gfredericks.test.chuck.properties
  "Alternative to clojure.test.check.properties."
  (:require [clojure.set :as sets]
            [clojure.test.check.properties :as prop
             #?@(:cljs [:include-macros true])]
            [com.gfredericks.test.chuck.generators :as gen'])
  #?(:cljs
     (:require-macros [com.gfredericks.test.chuck.properties])))

;; This namespace goes to a heck of a lot of effort just to get sane
;; args reported when a property fails. It semiduplicates syntactic
;; logic from both clojure.core/destructure and gen'/for. If you can
;; think of a better way to do this I'd love to hear it.

(defn ^:private symbol-name
  "foo/bar -> bar"
  [sym]
  (if (namespace sym)
    (symbol (name sym))
    sym))

(defn ^:private locals-in-binding-expr
  "Returns a set of symbols introduced in the given binding expression.
  Does not include gensyms introduced by destructuring.

  E.g., (locals-in-binding-expr '[[a {:keys [b c]}] & d]) => #{a b c d}"

  [expr]
  (cond (symbol? expr)
        #{expr}

        (vector? expr)
        (->> expr
             (remove #{'& :as})
             (map locals-in-binding-expr)
             (apply sets/union))

        (map? expr)
        (let [as (:as expr)
              things (map symbol-name (concat (:keys expr)
                                              (:syms expr)
                                              (:strs expr)))]
          (cond->
           (->> (keys expr)
                (remove #{:as :keys :syms :strs})
                (map locals-in-binding-expr)
                (concat [(set things)])
                (apply sets/union))
           as
           (conj as)))))

(declare for-bindings)

;;
;; At some point it would be cleaner to separate bindings used
;; directly with generators from bindings used with :let, and hide the
;; latter (in metadata?) so it doesn't come up as an arg to the
;; property
;;

(defn ^:private for-bindings-in-clause
  [left right]
  (cond (= :let left) (->> right
                           (partition 2)
                           (map first)
                           (mapcat locals-in-binding-expr))
        (= :when left) []
        (= :parallel left) (for-bindings right)
        (or (symbol? left) (map? left) (vector? left)) (locals-in-binding-expr left)
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
