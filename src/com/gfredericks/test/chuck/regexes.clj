(ns com.gfredericks.test.chuck.regexes
  "Generic regex analysis code, not test.check specific."
  (:require [instaparse.core :as insta]))

(def grammar-path "com/gfredericks/test/chuck/regex.bnf")

(def parse
  (-> grammar-path
      (clojure.java.io/resource)
      (slurp)
      (insta/parser :output-format :enlive)))

(defmulti analyze* :type)

(defmethod analyze* :regex
  [m]
  (analyze (assoc m :type :sequence)))

(defmethod analyze* :sequence
  [m]
  ;; most of the work to be done here...
  ;; probably want to start by splitting on the pipes?
  ())

(defmethod analyze* :default
  [m]
  (throw (ex-info "Missing analyze* dispatch!" {:arg m})))

(defn analyze
  "Should return a data structure that describes the semantics of the regex, having
  thrown away the syntactic details."
  [parse-tree]
  (analyze*
   (insta/transform
    {:Regex (fn [& contents] {:type :regex :contents contents})
     :LiteralChar (fn [c]
                    {:type :literal, :char c})
     :EscapedChar (fn [_slash [c]] c)
     :ParentheticalExpr (fn [_open & more]
                          {:type :group
                           :contents (butlast more)})
     :NotSlashE (fn [[c]] {:type :literal, :char c})
     :Expr identity
     :BaseExpr identity
     :PlainChar first}
    parse-tree)))

(-> "ab(cdf)\\Qhey"
    parse
    analyze)
