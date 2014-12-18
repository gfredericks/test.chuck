(ns com.gfredericks.test.chuck.regexes
  "Generic regex analysis code, not test.check specific."
  (:require [clojure.set :as set]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.regexes.charsets :as charsets]
            [instaparse.core :as insta]))

;;
;; Before releasing:
;;  - those unicode block names are actually a lot more than listed
;;    - and there's different categories of them and several syntaxes for
;;      specfifying them
;;

(def grammar-path "com/gfredericks/test/chuck/regex.bnf")

(defn ^:private parse-bigint [^String s] (clojure.lang.BigInt/fromBigInteger (java.math.BigInteger. s)))

(defn ^:private first! [coll] {:pre [(= 1 (count coll))]} (first coll))

(defn ^:private analyze-range
  [begin end]
  {:type :range
   :range [begin end]})

(defn ^:private remove-QE
  "Preprocesses a regex string (the same way that openjdk does) by
  transforming all \\Q...\\E expressions. Returns a string which is
  an equivalent regex that contains no \\Q...\\E expressions."
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
                   (if (or (re-matches #"[0-9a-zA-Z]" (str c1))
                           (<= 128 (int c1) ))
                     (cons c1 (remove-QE-quoting cs))
                     (list* \\ c1 (remove-QE-quoting cs)))))))]
      (apply str (remove-QE-not-quoting s)))
    s))

(def normal-slashed-characters
  {\t \tab, \n \newline, \r \return, \f \formfeed, \a \u0007, \e \u001B})

(defn ^:private unsupported
  [feature]
  {:type :unsupported,
   :unsupported #{feature}})

(defn analyze
  [parsed-regex]
  (insta/transform
   {:Regex identity
    :Alternation (fn [& regexes]
                   {:type     :alternation
                    :elements regexes})
    :Concatenation (fn [& regexes]
                     (cond-> {:type     :concatenation
                              :elements (->> regexes
                                             (remove nil?)
                                             (remove #{:flags}))}
                             (some #{:flags} regexes)
                             (assoc :unsupported #{:flags})))
    :MutatingMatchFlags (constantly :flags)
    :SuffixedExpr (fn
                    ([regex] regex)
                    ([regex {:keys [bounds quantifier]}]
                       (cond-> {:type     :repetition
                                :elements [regex]
                                :bounds   bounds}
                               quantifier
                               (assoc :unsupported
                                 #{:quantifiers}))))
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
                         ([group-flags alternation]
                            (assoc (unsupported :flags)
                              :elements [alternation])))
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
    :Anchor (constantly (unsupported :anchors))

    :Dot (constantly {:type :class, :simple-class :dot})
    :SpecialCharClass (fn [[c]]
                        {:type :class
                         :simple-class c})

    ;; If we want to support these do we need to be able to detect ungenerateable
    ;; expressions such as #"((x)|(y))\2\3"?
    :BackReference (constantly (unsupported :backreferences))

    :BCC (fn [intersection & [dangling-ampersands]]
           (cond-> {:type :class
                    :elements [intersection]
                    :brackets? true}
                   dangling-ampersands
                   (assoc :undefined #{:dangling-ampersands})))
    :BCCIntersection (fn [& unions]
                       {:type :class-intersection
                        :elements unions})
    :BCCUnionLeft (fn [& els]
                    (let [[negated? els] (if (= "^" (first els))
                                           [true (rest els)]
                                           [false els])]
                      {:type :class-union
                       :elements
                       (if negated?
                         [(cond->
                           {:type :class-negation
                            :elements [{:type :class-union
                                        :elements els}]}
                           ;; the jvm's behavior here seems pretttty weird:
                           ;; compare #"[^[x]]" and #"[^[x]x]"
                           (some :brackets? els)
                           (assoc :undefined #{"Character classes nested in negations"}))]
                         els)}))
    :BCCNegation identity
    :BCCUnionNonLeft (fn self [& els]
                       (if (and (string? (first els)) (re-matches #"&+" (first els)))
                         ;; This is undefined because compare:
                         ;;   - (re-seq #"[a-f&&&&c-h]" "abcdefghijklm")
                         ;;   - (re-seq #"[^a-f&&&&c-h]" "abcdefghijklm")
                         ;;   - (re-seq #"[^a-f&&c-h]" "abcdefghijklm")
                         (update-in (apply self (rest els))
                                    [:undefined]
                                    (fnil conj #{})
                                    "Character set intersections with more than two &'s")
                         {:type :class-union
                          :elements els}))
    :BCCElemHardLeft (fn self
                       ([x]
                          (if (= x "]")
                            {:type :character, :character \]}
                            x))
                       ([amps x]
                          (update-in (self x)
                                     [:undefined]
                                     (fnil conj #{})
                                     "Leading double ampersands")))
    :BCCElemLeft identity
    :BCCElemNonLeft identity
    :BCCElemBase (fn [x] (if (= :character (:type x))
                           {:type :class-base, :chars #{(:character x)}}
                           x))
    :BCCRange analyze-range
    :BCCRangeWithBracket #(analyze-range {:type :character, :character \]} %)
    :BCCChar identity
    :BCCCharNonRange identity
    :BCCCharEndRange identity
    :BCCDash (constantly {:type :character, :character \-})
    :BCCPlainChar (fn [[c]] {:type :character, :character c})
    :BCCOddAmpersands (constantly {:type :character, :character \&})
    :BCCAmpersandEndRange (constantly {:type :character, :character \&})

    :EscapedChar identity
    :NormalSlashedCharacters (fn [[_slash c]]
                               {:type :character
                                :character (normal-slashed-characters c)})

    ;; Apparently \v means \u000B, at least according to my experiments
    :WhatDoesThisMean (constantly {:type :character
                                   :character \u000B})

    :BasicEscapedChar (fn [[c]] {:type :character
                                 :character c})

    :HexChar (fn [hex-string]
               (let [n (BigInteger. hex-string 16)]
                 (when (> n Character/MAX_CODE_POINT)
                   (throw (ex-info "Bad hex character!"
                                   {:type ::parse-error
                                    :hex-string hex-string})))
                 (if (> n 16rFFFF)
                   {:type :large-unicode-character
                    :unsupported "large unicode characters"
                    :n n}
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
                             ;; TODO: this is not a complete list
                             (if (#{"C" "L" "M" "N" "P" "S" "Z"
                                    "{Lower}" "{Upper}" "{ASCII}"
                                    "{Alpha}" "{Digit}" "{Alnum}"
                                    "{Punct}" "{Graph}" "{Print}"
                                    "{Blank}" "{Cntrl}" "{XDigit}"
                                    "{Space}" "{javaLowerCase}"
                                    "{javaUpperCase}" "{javaWhitespace}"
                                    "{javaMirrored}" "{IsLatin}" "{InGreek}"
                                    "{Lu}" "{IsAlphabetic}" "{Sc}"} name)
                               (unsupported :unicode-character-classes)
                               (throw (ex-info "Bad unicode character class!"
                                               {:type ::parse-error
                                                :class-name name}))))}

   parsed-regex))

(def the-parser
  (-> grammar-path
      (clojure.java.io/resource)
      (slurp)
      (insta/parser)))

(defn throw-parse-errors
  "Checks the analyzed tree for any problems that cause exceptions
  with re-pattern."
  [analyzed-tree]
  (doseq [m (tree-seq #(contains? % :elements) :elements analyzed-tree)]
    (case (:type m)
      :range
      (let [[m1 m2] (:range m)
            c1 (or (some-> m1 :character int) (:n m1))
            c2 (or (some-> m2 :character int) (:n m2))]
        (when (and (integer? c1) (integer? c2) (< c2 c1))
          (throw (ex-info "Bad character range"
                          {:type ::parse-error
                           :range [c1 c2]}))))
      nil)))

(defn parse
  [s]
  (let [[the-parse & more :as ret] (insta/parses the-parser (remove-QE s))]
    (cond (nil? the-parse)
          (throw (ex-info "Parse failure!"
                          {:type ::parse-error
                           :instaparse-data (meta ret)}))

          #_ #_ ;; disabling this until that instaparse bug is fixed
          (seq more)
          (throw (ex-info "Ambiguous parse!"
                          {:type ::ambiguous-grammar
                           :parses ret}))

          :else
          (doto (analyze the-parse)
            (throw-parse-errors)))))

(defmulti ^:private compile-class
  "Takes a character class from the parser and returns a set of
  characters."
  :type)

(defmethod compile-class :class
  [m]
  (if-let [type (:simple-class m)]
    (case type
      :dot charsets/all-unicode-but-line-terminators
      (\d \D \s \S \w \W) (charsets/predefined type))
    (-> m :elements first! compile-class)))

(defmethod compile-class :class-intersection
  [m]

  (->> (:elements m)
       (map compile-class)
       (reduce charsets/intersection)))

(defmethod compile-class :class-union
  [m]
  (->> (:elements m)
       (map compile-class)
       (reduce charsets/union)))

(defmethod compile-class :class-negation
  [m]
  (let [[class] (:elements m)]
    (charsets/difference charsets/all-unicode (compile-class class))))

(defmethod compile-class :range
  [{[begin end] :range}]
  (charsets/range (str (:character begin))
                  (str (:character end))))

(defmethod compile-class :class-base
  [m]
  (->> (:chars m)
       (map str)
       (map charsets/singleton)
       (reduce charsets/union)))

(defmethod compile-class :character
  [m]
  (charsets/singleton (str (:character m))))

(defmulti analyzed->generator :type)

(defmethod analyzed->generator :default
  [m]
  (throw (ex-info "No match in analyzed->generator!" {:arg m})))

(defmethod analyzed->generator :concatenation
  [{:keys [elements]}]
  (->> elements
       (map analyzed->generator)
       (doall)
       (apply gen/tuple)
       (gen/fmap #(apply str %))))

(defmethod analyzed->generator :alternation
  [{:keys [elements]}]
  (->> elements
       (map analyzed->generator)
       (doall)
       (gen/one-of)))

(defmethod analyzed->generator :character
  [{:keys [character]}]
  {:pre [character]}
  (gen/return (str character)))

(defmethod analyzed->generator :repetition
  [{:keys [elements bounds]}]
  (let [[lower upper] bounds]
    (assert lower)
    (let [g (analyzed->generator (first! elements))]
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

(defmethod analyzed->generator :class
  [class]
  (let [charset (compile-class class)
        size (charsets/size charset)]
    (if (zero? size)
      (throw (ex-info "Cannot generate characters from empty class!"
                      {:type ::ungeneratable}))
      (gen/fmap (partial charsets/nth charset)
                (gen/choose 0 (dec size))))))



(defmethod analyzed->generator :unsupported
  [{:keys [feature]}]
  (throw (ex-info "Unsupported regex feature!"
                  {:type ::unsupported-feature
                   :feature feature
                   :patches? "welcome."})))

(defn gen-string-from-regex
  [^java.util.regex.Pattern re]
  ;; this check helps catch flags like CANON_EQ that aren't
  ;; necessarily represented in the text of the regex
  (when (pos? (.flags re))
    (throw (ex-info "Unsupported feature"
                    {:type ::unsupported-feature
                     :feature "flags"})))
  (let [analyzed (-> re str parse)]
    (doseq [m (tree-seq #(contains? % :elements) :elements analyzed)]
      (when-let [[x] (seq (:unsupported m))]
        (throw (ex-info "Unsupported-feature"
                        {:type ::unsupported-feature
                         :feature x})))
      (when-let [[x] (seq (:undefined m))]
        (throw (ex-info "Undefined regex syntax"
                        {:type ::unsupported-feature
                         :feature x}))))
    (analyzed->generator analyzed)))
