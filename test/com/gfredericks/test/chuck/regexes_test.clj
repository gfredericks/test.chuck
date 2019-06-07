(ns com.gfredericks.test.chuck.regexes-test
  (:require [clojure.test :refer [deftest are]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.gfredericks.test.chuck :refer [times]]
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
      (gen'/for [:parallel [[open closed] (gen/elements [[nil nil]
                                                         ["(" ")"]
                                                         ["[" "]"]
                                                         ["[^" "]"]
                                                         ["{" "}"]
                                                         ["(?" ")"]
                                                         ["(?=" ")"]
                                                         ["(?!" ")"]
                                                         ["(?<=" ")"]
                                                         ["(?<!" ")"]])
                            els (gen/list g)]]
        (if open
          [open els closed]
          els)))
    (gen/one-of [gen/string gen-regexy-fragment]))))

(def gen-strings-that-might-be-regex-like
  "Generator for strings that are weighted toward special regex
  syntax."
  (gen/one-of [gen-regexy-string
               gen/string-ascii
               gen/string]))

(def regex-parse-error-exceptions
  "Parse error messages thrown by java.util.regex.Pattern that we
  don't mind the parser not catching, usually because it involves an
  unsupported feature."
  [;; it's too difficult to try to reproduce the exception-throwing
   ;; behavior of re-pattern for repetetive-lookbehind, especially
   ;; since lookbehind isn't a supported feature anyways.  E.g.,
   ;; #"(?<=xx*)" is a parse error but #"(?<=x*)" is okay for some
   ;; reason. Gets even more complicated if you nest lookahead inside
   ;; your lookbehind.
   #"Look-behind group does not have an obvious maximum length"])

(defn correct-parse-behavior?
  "Checks that our parser and re-pattern will either both successfully
  parse the string or will both throw an exception."
  [s]
  (let [[jvm-result err-msg] (try (re-pattern s)
                                  [:parses]
                                  (catch java.util.regex.PatternSyntaxException e
                                    [:throws (.getMessage e)]))
        [our-result] (try (regexes/parse s)
                          [:parses]
                          (catch clojure.lang.ExceptionInfo e
                            (if (= ::regexes/parse-error (:type (ex-data e)))
                              [:throws]
                              (throw e))))]
    (or (= jvm-result our-result)
        (and (= :throws jvm-result)
             (some #(re-find % err-msg) regex-parse-error-exceptions)))))

(deftest parser-regression
  (are [s] (correct-parse-behavior? s)
       ;; should always parse
       "[]-_]" "[-x]" "[x+--y]" "[\\e]" "\\\0" "[[x]-y]" "(?)"
       "[&&x]" "[x&&y]" "[x&]" "[x&&]" "[&]" "[--?]"
       "{0}?" "[\\c\n]" "[\\e- ]" "\\Q\\E" "[\\Q][\\E]"
       "(?:)" "[!-&&]" "\\c\\" "[\u0000-\\00]" "((?){0,0})"
       "[%-&&&]" "[x&&&&]" "(?<=)" "(?c:Z)"
       ;; should never parse
       "[b-a]" "[^]" "[]-X]" "[&&&]" "[\\Q\\E]" "(??)"
       "\\x{110000}" "{1,0}" "[[[[{-\\c}]]]]" "[x-\\cx]"
       "[{\\x{10000}-}]" "[b-a]??" "(?)?" "[\\R]"
       ;; parses in java 8 but not java 7
       "\\R" "\\H" "\\h" "\\V"))

(defspec parser-spec (times 1000)
  (prop/for-all [s gen-strings-that-might-be-regex-like]
    (correct-parse-behavior? s)))

(def gen-regex
  (let [maybes (gen/fmap #(try (re-pattern %)
                               (catch java.util.regex.PatternSyntaxException e))
                         gen-strings-that-might-be-regex-like)]
    (gen/such-that identity maybes 100)))

(def gen-generator-scenario
  (gen'/for [regex gen-regex
             :let [gen (try (regexes/gen-string-from-regex regex)
                            (catch Throwable e
                              (when (not
                                     (#{::regexes/unsupported-feature
                                        ::regexes/ungeneratable}
                                      (:type (ex-data e))))
                                (throw (ex-info "Craptastic" {:regex regex} e)))))]
             :when ^{:max-tries 100} gen
             s gen]
    {:regex regex, :s s}))

(defspec generator-spec (times 1000)
  (prop/for-all [{:keys [regex s]} gen-generator-scenario]
    (re-matches regex s)))

(def generator-regression-cases
  ["[\\c\\u]" "\\c\\Q\u0080" "\\c\\\u0080" "\\v"
   "[^aceg\\S]"
   "foo(?:bar)*" ;; should allow non-capturing groups
   "iddqd(?<x>idkfa)" ;; should allow named groups

   ;; intersection unsupported for now
   #_"[{&&[}{]}]"])

(defspec generator-regression-spec (times 1000)
  (prop/for-all [[re s] (gen'/for [re-s (gen/elements generator-regression-cases)
                                   :let [re (re-pattern re-s)]
                                   s (regexes/gen-string-from-regex re)]
                          [re s])]
    (re-matches re s)))

(deftest undefined-regression
  (are [s] (try (-> s re-pattern regexes/gen-string-from-regex)
                false
                (catch clojure.lang.ExceptionInfo e
                  (if (= "Undefined regex syntax" (.getMessage e))
                    true
                    (throw e))))
       "[&&x]" "[x&&]" "[^[x]]" "[^[x]x]" "[a&&&b]" "[x&&&&y]"
       "[^{}&&&]" "[x&&&&]" "[x&&&&&]"))
