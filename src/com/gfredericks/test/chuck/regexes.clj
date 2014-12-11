(ns com.gfredericks.test.chuck.regexes
  "Generic regex analysis code, not test.check specific."
  (:require [clojure.test.check.generators :as gen]
            [instaparse.core :as insta]))

(def grammar-path "com/gfredericks/test/chuck/regex.bnf")

(defn ^:private parse-bigint [^String s] (clojure.lang.BigInt/fromBigInteger (java.math.BigInteger. s)))

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
   {:Regex identity
    :Alternation (fn [& regexes]
                   {:type     :alternation
                    :elements regexes})
    :Concatenation (fn [& regexes]
                     {:type     :concatenation
                      :elements regexes})
    :SuffixedExpr (fn
                    ([regex] regex)
                    ([regex suffix]
                       (if (:quantifier suffix)
                         {:type :unsupported
                          :feature "quantifiers"}
                         {:type :repetition
                          :element regex
                          :bounds (:bounds suffix)})))
    :Suffix (fn
              ([bounds] {:bounds bounds})
              ([bounds quantifier] {:bounds bounds, :quantifier quantifier}))
    :Optional (constantly [0 1])
    :Positive (constantly [1 nil])
    :NonNegative (constantly [0 nil])
    :CurlyRepetition (fn
                       ([s] (let [n (parse-bigint s)] [n n]))
                       ([s _comma] [(parse-bigint s) nil])
                       ([s1 _comma s2]
                          {:post [(<= (first %) (second %))]}
                          [(parse-bigint s1) (parse-bigint s2)]))
    :ParenthesizedExpr (fn
                         ([alternation] alternation)
                         ([group-flags aternation]
                            {:type :unsupported
                             :feature "flags"}))
    :SingleExpr identity
    :BaseExpr identity
    :CharExpr identity
    :LiteralChar identity
    :PlainChar (fn [s] {:pre [(= 1 (count s))]}
                 {:type :character, :character (first s)})

    :Anchor (constantly {:type :unsupported
                         :feature "anchors"})

    :Dot (constantly {:type :unsupported
                      :feature "character classes"})
    :SpecialCharClass (constantly {:type :unsupported
                                   :feature "character classes"})

    :BCC (constantly {:type :unsupported
                      :feature "character classes"})

    :BackReference (constantly {:type :unsupported
                                :feature "Backreference"})

    :BCCRange analyze-range
    :BCCRangeWithBracket #(analyze-range \] %)
    :BCCChar identity
    :BCCDash first
    :BCCPlainChar first
    :BCCOddAmpersands first

    :EscapedChar identity
    :NormalSlashedCharacters (fn [[_slash c]]
                               {:type :character
                                :character (normal-slashed-characters c)})
    :BasicEscapedChar (fn [[c]] {:type :character
                                 :character c})
    :OctalChar (fn [& strs]
                 {:type :character
                  :character (char (read-string (apply str "8r" strs)))})
    :OctalDigits1 list
    :OctalDigits2 list
    :OctalDigits3 list}

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

(defmethod analyzed->generator :character
  [{:keys [character]}]
  (gen/return (str character)))

(defmethod analyzed->generator :repetition
  [{:keys [element bounds]}]
  (let [[lower upper] bounds]
    (assert lower)
    (let [g (analyzed->generator element)]
      (gen/fmap #(apply str %)
                (if (= lower upper)
                  (gen/vector g lower)
                  (if upper
                    (gen/vector g lower upper)
                    ;; what about the lower!
                    (if (zero? lower)
                      (gen/vector g)
                      (gen/fmap #(apply concat %)
                                (gen/tuple (gen/vector g lower)
                                           (gen/vector g))))))))))

(defmethod analyzed->generator :unsupported
  [{:keys [feature]}]
  (throw (ex-info "Unsupported regex feature!"
                  {:type ::unsupported-feature
                   :feature feature
                   :patches? "welcome."})))
