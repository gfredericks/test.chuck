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

(defn ^:private remove-QE
  [^String s]
  (if (.contains s "\\Q")
    (letfn [(remove-QE-not-quoting [chars]
              (lazy-seq
               (when-let [[c1 & [c2 :as cs]] (seq chars)]
                 (if (= \\ c1)
                   (if (= \Q c2)
                     (remove-QE-quoting (rest cs))
                     (list* c1 c2 (remove-QE-not-quoting (rest cs))))
                   (cons c1 (remove-QE-not-quoting cs))))))
            (remove-QE-quoting [chars]
              (lazy-seq
               (when-let [[c1 & [c2 :as cs]] (seq chars)]
                 (if (and (= c1 \\) (= c2 \E))
                   (remove-QE-not-quoting (rest cs))
                   (if (re-matches #"[0-9a-zA-Z]" (str c1))
                     (cons c1 (remove-QE-quoting cs))
                     (list* \\ c1 (remove-QE-quoting cs)))))))]
      (apply str (remove-QE-not-quoting s)))
    s))

(def normal-slashed-characters
  {\t \tab, \n \newline, \r \return, \f \formfeed, \a \u0007, \e \u001B})

(defn analyze
  [parsed-regex]
  (insta/transform
   {:BCCRange analyze-range
    :BCCRangeWithBracket #(analyze-range \] %)
    :BCCChar identity
    :BCCDash first
    :BCCPlainChar first
    :BCCOddAmpersands first
    :EscapedChar identity
    :NormalSlashedCharacters (fn [[_slash c]]
                               (normal-slashed-characters c))
    :BasicEscapedChar first}

   parsed-regex))

(def the-parser
  (-> grammar-path
      (clojure.java.io/resource)
      (slurp)
      (insta/parser)))

(defn parse
  [s]
  (let [[the-parse & more :as ret] (insta/parses the-parser (remove-QE s))]
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
