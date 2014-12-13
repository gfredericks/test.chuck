(ns com.gfredericks.test.chuck.regexes-test
  (:require [clojure.test :refer [deftest are]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.gfredericks.test.chuck.regexes :as regexes]
            [com.gfredericks.test.chuck.generators :as gen']
            [instaparse.core :as insta]))

;; Difficult to generate: parsable references to named capturing groups
(def gen-regexy-fragment
  (gen/frequency
   [[10 (gen/elements (concat "?*+!|()[]{}<>=^$.\\:-&"
                              ["\\Q" "\\E" "\\c" "&&" "\\p"]))]
    [1 (gen'/for [chars (gen/list (gen/choose 0 7))]
         (apply str "\\0" chars))]
    [1 (gen'/for [:parallel [p (gen/elements "pP")
                             class-name (gen/elements
                                         ["Lower" "Upper" "ASCII" "Alpha" "Digit"
                                          "Alnum" "Punct" "Graph" "Print" "Blank"
                                          "Cntrl" "XDigit" "Space" "javaLowerCase"
                                          "javaUpperCase" "javaWhitespace"
                                          "javaMirrored" "IsLatin" "InGreek" "Lu"
                                          "IsAlphabetic" "Sc"])]]
         (format "\\%s{%s}" p class-name))]
    [1 (gen'/for [:parallel
                  [ux (gen/elements "ux")
                   chars (gen/list (gen/elements "0123456789abcdef"))]]
         (apply str "\\" ux chars))]
    [1 (gen'/for [chars (gen/list (gen/elements "0123456789abcdef"))]
         (format "\\x{%s}" (apply str chars)))]
    [1 (gen'/for [d gen/nat
                  s (gen/elements ["{%d}" "{%d,}"])]
         (format s d))]
    [1 (gen'/for [:parallel [d1 gen/nat
                             d2 gen/nat]]
         (format "{%d,%d}" d1 d2))]]))

(def gen-regexy-string
  (gen/fmap
   (fn [expr]
     (->> expr
          (tree-seq coll? seq)
          (remove coll?)
          (apply str)))
   (gen/recursive-gen
    (fn [g]
      (gen'/for [:parallel [[open closed] (gen/elements [["[" "]"]
                                                         ["[^" "]"]
                                                         ["{" "}"]
                                                         ["(" ")"]
                                                         ["(?" ")"]
                                                         [nil nil]])
                            els (gen/list g)]]
        (if open
          [open els closed]
          els)))
    (gen/one-of [gen/string gen-regexy-fragment]))))

(def gen-strings-that-might-be-regex-like
  (gen/one-of [gen-regexy-string
               gen/string-ascii
               gen/string]))

(def gen-regex-parsing-attempt
  (gen'/for [s gen-strings-that-might-be-regex-like]
    (try (do
           (re-pattern s)
           [:parsed s])
         (catch java.util.regex.PatternSyntaxException e
           [:not-parsed s]))))

(defn parses?
  [s]
  (try (pr-str (regexes/parse s)) ;; something lazy going on here
       true
       (catch clojure.lang.ExceptionInfo e
         (if (= ::regexes/parse-error (:type (ex-data e)))
           false
           (throw e)))))

(deftest parser-regression
  (are [s] (parses? s)
       "[]-_]" "[-x]" "[x+--y]" "[\\e]" "\\\0" "[[x]-y]" "(?)"
       "[&&x]" "[x&&y]" "[x&]" "[x&&]" "[&]" "[--?]"
       "{0}?" "[\\c\n]" "[\\e- ]" "\\Q\\E" "[\\Q][\\E]"
       "(?:)" "[!-&&]")
  (are [s] (not (parses? s))
       "[b-a]" "[^]" "[]-X]" "[&&&]" "[\\Q\\E]" "(??)"
       "\\x{110000}" "{1,0}" "[[[[{-\\c}]]]]" "[x-\\cx]"))

(defspec parser-spec 1000
  (prop/for-all [[flag s] gen-regex-parsing-attempt]
    (let [parsed? (parses? s)]
      (or (= [flag parsed?] [:parsed true])
          (= [flag parsed?] [:not-parsed false])))))

(def gen-regex
  (let [maybes (gen/fmap #(try (re-pattern %)
                               (catch java.util.regex.PatternSyntaxException e))
                         gen-strings-that-might-be-regex-like)]
    (gen/such-that identity maybes 100)))

(def gen-generator-scenario
  (gen'/for [regex gen-regex
             :let [gen (try (-> regex str regexes/parse regexes/analyzed->generator)
                            (catch clojure.lang.ExceptionInfo e
                              (when (not
                                     (#{:regexes/unsupported-feature
                                        :regexes/ungeneratable}
                                      (:type (ex-data e))))
                                (throw (ex-info "Craptastic" {:regex regex} e)))))]
             :when ^{:max-tries 100} gen
             s gen]
    {:regex regex, :s s}))

(defspec generator-spec 1000
  (prop/for-all [{:keys [regex s]} gen-generator-scenario]
    (re-matches regex s)))
