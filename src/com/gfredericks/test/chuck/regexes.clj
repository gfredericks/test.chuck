(ns com.gfredericks.test.chuck.regexes
  "Generic regex analysis code, not test.check specific."
  (:require [instaparse.core :as insta]))

(def grammar-path "com/gfredericks/test/chuck/regex.bnf")

(defn ^:private analyze-range
  [begin end]
  {:pre [(char? begin) (char? end)]}
  (when (< (int end) (int begin))
    (throw (ex-info "Parse failure!"
                    {:type ::parse-error
                     :character-class-range [begin end]})))
  {:type :range, :begin begin, :end end})

(defn analyze
  [parsed-regex]
  (insta/transform
   {:BCCRange analyze-range
    :BCCRangeWithBracket #(analyze-range \] %)
    :BCCRangeWithDash #(analyze-range \- %)
    :BCCChar identity
    :BCCDash first
    :BCCPlainChar first
    :BCCAmpersand first
    :BasicEscapedChar first}

   parsed-regex))

(def the-parser
  (-> grammar-path
      (clojure.java.io/resource)
      (slurp)
      (insta/parser)))

(defn parse
  [s]
  (let [[the-parse & more :as ret] (insta/parses the-parser s)]
    (cond (nil? the-parse)
          (throw (ex-info "Parse failure!"
                          {:type ::parse-error
                           :instaparse-data (meta ret)}))

          (seq more)
          (throw (ex-info "Ambiguous parse!"
                          {:type ::ambiguous-grammar
                           :parses ret}))

          :else
          (analyze the-parse))))
