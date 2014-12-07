(ns com.gfredericks.test.chuck.regexes
  "Generic regex analysis code, not test.check specific."
  (:require [instaparse.core :as insta]))

(def grammar-path "com/gfredericks/test/chuck/regex.bnf")

(defn analyze
  [parsed-regex]
  (insta/transform
   {:BCCRange (fn [begin end]
                {:pre [(char? begin) (char? end)]}
                (when (< (int end) (int begin))
                  (throw (ex-info "Parse failure!"
                                  {:type ::parse-error
                                   :character-class-range [begin end]})))
                {:type :range, :begin begin, :end end})
    :BCCPlainChar first
    :BCCChar identity}
   parsed-regex))

(def parse
  (comp
   analyze
   #(if (insta/failure? %)
      (throw (ex-info "Parse failure!"
                      {:type ::parse-error
                       :instaparse-data %}))
      %)
   (-> grammar-path
       (clojure.java.io/resource)
       (slurp)
       (insta/parser))))
