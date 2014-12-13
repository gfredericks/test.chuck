(ns com.gfredericks.test.chuck.regexes
  "Generic regex analysis code, not test.check specific."
  (:require [clojure.test.check.generators :as gen]
            [instaparse.core :as insta]))

;;
;; Before releasing:
;;  - read through the Pattern docs and make sure every syntactic
;;    construct mentioned is represented in the generators
;;    - unicode block names?
;;  - add a character set concept for negated character classes and DOT
;;  - add a check for that CANON_EQ flag (which I can't imagine supporting)
;;    - actually we should probably check ALL the flags since I think they
;;      can be given that way
;;  - those unicode block names are actually a lot more than listed
;;

(def grammar-path "com/gfredericks/test/chuck/regex.bnf")

(defn ^:private parse-bigint [^String s] (clojure.lang.BigInt/fromBigInteger (java.math.BigInteger. s)))

(defn ^:private analyze-range
  [begin end]
  (let [{btype :type} begin
        {etype :type} end]
    (cond (= :unsupported btype) begin
          (= :unsupported etype) end

          (not (and (= :character btype etype)))
          (throw (ex-info "Unexpected error parsing range."
                          {:begin begin :end end}))

          :else
          (let [begin (:character begin)
                end (:character end)]
            (when (< (int end) (int begin))
              (throw (ex-info "Parse failure!"
                              {:type ::parse-error
                               :character-class-range [begin end]})))
            {:type :range, :begin begin, :end end}))))

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

(defn ^:private unsupported
  [feature]
  {:type :unsupported, :feature feature})

(defn analyze
  [parsed-regex]
  (insta/transform
   {:Regex identity
    :Alternation (fn [& regexes]
                   {:type     :alternation
                    :elements regexes})
    :Concatenation (fn [& regexes]
                     {:type     :concatenation
                      ;; maybe nil because of DanglingCurlyRepetitions
                      :elements (remove nil? regexes)})
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
              ;; this function can get a nil 2nd or 3rd arg because of
              ;; DanglingCurlyRepetitions, which we don't hide so we
              ;; get more parse error coverage, e.g. for #"{1,0}"
              [bounds & [quantifier]]
              (cond-> {:bounds bounds}
                      quantifier
                      (assoc :quantifier quantifier)))
    :Optional (constantly [0 1])
    :Positive (constantly [1 nil])
    :NonNegative (constantly [0 nil])
    :CurlyRepetition (fn
                       ([s] (let [n (parse-bigint s)] [n n]))
                       ([s _comma] [(parse-bigint s) nil])
                       ([s1 _comma s2]
                          (let [lower (parse-bigint s1)
                                upper (parse-bigint s2)]
                            (when (< upper lower)
                              (throw (ex-info "Bad repetition range"
                                              {:type ::parse-error
                                               :range [lower upper]})))
                            [lower upper])))
    ;; the only reason we don't hide this in the parser is to force
    ;; the "Bad reptition range" check above, so that our "parses
    ;; exactly what re-pattern does" spec will pass.
    :DanglingCurlyRepetitions (constantly nil)
    :ParenthesizedExpr (fn
                         ([alternation] alternation)
                         ([group-flags aternation] (unsupported "flags")))
    :SingleExpr identity
    :BaseExpr identity
    :CharExpr identity
    :LiteralChar identity
    :PlainChar (fn [s] {:pre [(= 1 (count s))]}
                 {:type :character, :character (first s)})

    :ControlChar (fn [[c]]
                   ;; this is the same calculation openjdk performs so
                   ;; it must be right.
                   {:type :character, :character (-> c int (bit-xor 64) char)})

    ;; sounds super tricky. looking forward to investigating
    :Anchor (constantly (unsupported "anchors"))

    ;; need to figure out if there's a canonical character set to draw
    ;; from
    :Dot (constantly (unsupported "character classes"))
    :SpecialCharClass (constantly (unsupported "character classes"))

    ;; can do these as soon as I come up with some nice range algebra
    :BCC (constantly (unsupported "character classes"))

    :BackReference (constantly (unsupported "backreferences"))

    :BCCRange analyze-range
    :BCCRangeWithBracket #(analyze-range {:type :character, :character \]} %)
    :BCCChar identity
    :BCCCharEndRange identity
    :BCCDash (constantly {:type :character, :character \-})
    :BCCPlainChar (fn [[c]] {:type :character, :character c})
    :BCCOddAmpersands (constantly {:type :character, :character \&})
    :BCCEvenAmpersands (constantly {:type :character, :character \&})

    :EscapedChar identity
    :NormalSlashedCharacters (fn [[_slash c]]
                               {:type :character
                                :character (normal-slashed-characters c)})
    :WhatDoesThisMean (constantly (unsupported "what is \\v?"))
    :BasicEscapedChar (fn [[c]] {:type :character
                                 :character c})

    :HexChar (fn [hex-string]
               (let [n (BigInteger. hex-string 16)]
                 (when (> n Character/MAX_CODE_POINT)
                   (throw (ex-info "Bad hex character!"
                                   {:type ::parse-error
                                    :hex-string hex-string})))
                 (if (> n 16rFFFF)
                   (unsupported "large unicode characters")
                   {:type :character
                    :character (char (int n))})))
    :ShortHexChar identity
    :MediumHexChar identity
    :LongHexChar identity

    :OctalChar (fn [strs]
                 {:type :character
                  :character (char (read-string (apply str "8r" strs)))})
    :OctalDigits1 list
    :OctalDigits2 list
    :OctalDigits3 list

    :UnicodeCharacterClass (fn [p name]
                             (if (#{"C" "L" "M" "N" "P" "S" "Z"
                                    "{Lower}" "{Upper}" "{ASCII}"
                                    "{Alpha}" "{Digit}" "{Alnum}"
                                    "{Punct}" "{Graph}" "{Print}"
                                    "{Blank}" "{Cntrl}" "{XDigit}"
                                    "{Space}" "{javaLowerCase}"
                                    "{javaUpperCase}" "{javaWhitespace}"
                                    "{javaMirrored}" "{IsLatin}" "{InGreek}"
                                    "{Lu}" "{IsAlphabetic}" "{Sc}"} name)
                               (unsupported "unicode character classes")
                               (throw (ex-info "Bad unicode character class!"
                                               {:type ::parse-error
                                                :class-name name}))))}

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
  {:pre [character]}
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
