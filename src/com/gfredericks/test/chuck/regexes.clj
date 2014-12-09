(ns com.gfredericks.test.chuck.regexes
  "Generic regex analysis code, not test.check specific."
  (:require [clojure.test.check.generators :as gen]
            [instaparse.core :as insta]))

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
    :BCCChar identity
    :BCCDash first
    :BCCPlainChar first
    :BCCOddAmpersands first
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

(defmulti analyzed->generator :type)

(defmethod analyzed->generator :default
  [m]
  (throw (ex-info "No match in analyzed->generator!" {:arg m})))

(defmethod analyzed->generator :concatenation
  [{:keys [elements]}]
  (->> elements
       (map analyzed->generator)
       (apply gen/tuple)
       (gen/fmap #(apply str %))))

(defmethod analyzed->generator :alternation
  [{:keys [elements]}]
  (->> elements
       (map analyzed->generator)
       (gen/one-of)))
