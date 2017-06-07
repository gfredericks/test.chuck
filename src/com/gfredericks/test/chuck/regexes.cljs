(ns com.gfredericks.test.chuck.regexes
  "Internals of the string-from-regex generator for Clojurescript."
  (:require [instaparse.core :as insta :refer-macros [defparser]]
            [cljs.reader :refer [read-string]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.regexes.charsets :as charsets]))

(defn ^:private parse-bigint
  [s]
  (js/parseInt s))

(defn ^:private first! [coll] {:pre [(= 1 (count coll))]} (first coll))

(defn ^:private analysis-tree-seq
  [tree]
  (tree-seq #(contains? % :elements) :elements tree))

(defparser the-parser "./resources/com/gfredericks/test/chuck/regex-cljs.bnf")

(defn ^:private re?
  "Checks if the string compiles with re-pattern."
  [s]
  (try (re-pattern s)
       true
       (catch js/SyntaxError e
         false)))

(defn ^:private analyze-range
  [begin end]
  {:type :range
   :elements [begin end]})

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
                     ([regex]
                      (if (and (vector? regex)
                               (= 2 (count regex))
                               (not (keyword? (first regex))))
                        (throw (ex-info "Invalid regular expression, nothing to repeat"
                                        {:type ::parse-error
                                         :expr regex})))
                      regex)
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
     :BCCEmpty (constantly ::empty-class)

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

     :ParenthesizedExpr (fn
                          ([alternation]
                           {:type :group
                            :elements [alternation]})
                          ([group-flags alternation]
                           (cond-> {:type :group
                                    :elements [alternation]
                                    :flag group-flags}
                             ;; If the flags are just an empty
                             ;; NonCapturingMatchFlags, we can safely
                             ;; ignore it; this should correspond to
                             ;; something like #"foo(?:bar)*"
                             (not= group-flags
                                   [:GroupFlags [:NonCapturingMatchFlags]])
                             (assoc :unsupported #{:flags}))))
     :SingleExpr identity
     :LinebreakMatcher (constantly
                         {:type :alternation
                          :elements [{:type :concatenation
                                      :elements [{:type :character
                                                  :character \return}
                                                 {:type :character
                                                  :character \newline}]}
                                     {:type :class
                                      :simple-class :R}]
                          :feature :backslash-R})
     :BaseExpr identity
     :CharExpr identity
     :LiteralChar identity
     :PlainChar (fn [s] {:pre [(= 1 (count s))]}
                  {:type :character, :character (first s)})

     :LiteralSpecialChar (fn [s] {:pre [(= 1 (count s))]}
                  {:type :character, :character (first s)})

     :LiteralCurly (fn [s]
                     {:type :character, :character "{}"})

     :ControlChar (fn [[c]]
                    ;; this is the same calculation openjdk performs so
                    ;; it must be right.
                    {:type :character, :character (-> c int (bit-xor 64) char)})

     ;; sounds super tricky. looking forward to investigating
     :Anchor (constantly (unsupported :anchors))

     :Dot (constantly {:type :class, :simple-class :dot})
     :SpecialCharClass (fn [[c]]
                         (cond-> {:type :class
                                  :simple-class c}
                           (#{\V \h \H} c)
                           (assoc :feature :HV-classes)))

     ;; If we want to support these do we need to be able to detect ungenerateable
     ;; expressions such as #"((x)|(y))\2\3"?
     :BackReference (constantly (unsupported :backreferences))

     :BCC (fn [union]
            {:type :class
             :elements (cond-> []
                         union (conj union))
             :brackets? true})

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
     :BCCPlainAmpersand (constantly {:type :character, :character \&})

     :EscapedChar identity
     :NormalSlashedCharacters (fn [[_slash c]]
                                {:type :character
                                 :character (normal-slashed-characters c)})

     :BasicEscapedChar (fn [[c]] {:type :character
                                  :character c})

     :HexChar (fn [hex-string]
                (let [n (js/parseInt hex-string 16)]
                  (when (> n 1114111)
                    (throw (ex-info "Bad hex character!"
                                    {:type ::parse-error
                                     :hex-string hex-string})))
                  (if (> n 16rFFFF)
                    {:type :large-unicode-character
                     :unsupported #{"large unicode hex literals"}
                     :n n}
                    {:type :character
                     :character (char (int n))})))
     :ShortHexChar identity
     :MediumHexChar identity
     :LongHexChar identity
     :VeryLongHexChar identity}

    parsed-regex))

(defn throw-parse-errors
  "Checks the analyzed tree for any problems that cause exceptions
  with re-pattern."
  [analyzed-tree]
  (let [input-string (::instaparse-input (meta analyzed-tree))]
    (doseq [m (analysis-tree-seq analyzed-tree)]
      (case (:type m)
        :range
        (let [[m1 m2] (:elements m)
              c1 (or (some-> m1 :character (.charCodeAt 0)) (:n m1))
              c2 (or (some-> m2 :character (.charCodeAt 0)) (:n m2))]
          (when (and (integer? c1) (integer? c2) (< c2 c1))
            (throw (ex-info "Bad character range"
                            {:type ::parse-error
                             :range [c1 c2]}))))
        nil))))

(defn parse-raw [s]
  (insta/parse the-parser s))

(defn parse [s]
  (let [[the-parse & more :as ret] (insta/parses the-parser s)]
    (cond (nil? the-parse)
          (throw (ex-info "Parse failure!"
                          {:type            ::parse-error
                           :instaparse-data (meta ret)}))

          :else
          (-> the-parse
              (analyze)
              (vary-meta assoc ::instaparse-input s)
              (doto (throw-parse-errors))))))

(defmulti ^:private compile-class
          "Takes a character class from the parser and returns a set of
          characters."
          :type)

(defmethod compile-class :class
  [m]
  (if-let [type (:simple-class m)]
    (case type
      :dot charsets/all-unicode-but-line-terminators
      \v (charsets/singleton "\u000B")
      (\d \D \s \S \w \W) (charsets/predefined-regex-classes type))
    (if (empty? (:elements m))
      (charsets/singleton "")
      (-> m :elements first! compile-class))))

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
  [{[begin end] :elements}]
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
  (if (contains? (set elements) ::empty-class)
    (gen/return "")
    (->> elements
         (map analyzed->generator)
         (doall)
         (apply gen/tuple)
         (gen/fmap #(apply str %)))))

(defmethod analyzed->generator :alternation
  [{:keys [elements]}]
  (->> elements
       (remove #{::empty-class})
       (map analyzed->generator)
       (doall)
       (gen/one-of)))

(defmethod analyzed->generator :group
  [{:keys [elements]}]
  (-> elements first! analyzed->generator))

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
                (cond (= lower upper)
                      (gen/vector g lower)

                      upper
                      (gen/vector g lower upper)

                      (zero? lower)
                      (gen/vector g)

                      :else
                      (gen/fmap #(apply concat %)
                                (gen/tuple (gen/vector g lower)
                                           (gen/vector g))))))))

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
  "Takes a regex and returns a generator for strings that match it."
  [re]
  ;; this check helps catch flags like CANON_EQ that aren't
  ;; necessarily represented in the text of the regex
  (when (pos? (count (.-flags re)))
    (throw (ex-info "Unsupported feature"
                    {:type ::unsupported-feature
                     :feature "flags"})))
  (let [analyzed (-> re str parse)
        parser-input (::instaparse-input (meta analyzed))]
    (doseq [m (analysis-tree-seq analyzed)]
      (when-let [[x] (seq (:unsupported m))]
        (throw (ex-info "Unsupported-feature"
                        {:type ::unsupported-feature
                         :feature x})))
      (when-let [[x] (seq (:undefined m))]
        (throw (ex-info "Undefined regex syntax"
                        {:type ::unsupported-feature
                         :feature x})))
      (when (:brackets? m)
        (let [[begin end] (insta/span m)
              s (subs parser-input begin end)]
          (when (re-find #"\[&&|&&&|&&]" s)
            (throw (ex-info "Undefined regex syntax"
                            {:type ::unsupported-feature
                             :feature "Ambiguous use of & in a character class"}))))))
    (analyzed->generator analyzed)))
