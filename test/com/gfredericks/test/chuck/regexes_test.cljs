(ns com.gfredericks.test.chuck.regexes-test
  (:require [cljs.test :refer-macros [deftest is are] :include-macros true]
            [clojure.test.check]
            [clojure.test.check.clojure-test :refer-macros [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]
            [com.gfredericks.test.chuck :refer [times]]
            [com.gfredericks.test.chuck.generators :as gen']
            [com.gfredericks.test.chuck.regexes :as regexes]
            [goog.string :as gstr]))

(defn format [& args] (apply gstr/format args))

(def gen-regexy-fragment
  (gen/frequency
    [[10 (gen/elements (concat "?*+!|()[]{}<>=^$.\\:-&"
                               ["\\E" "\\c" "\\p"]))]
     [1 (gen'/for [chars (gen/list (gen/choose 0 7))]
          (apply str "\\0" chars))]
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
                                                           ["(?:" ")"]
                                                           ["(?=" ")"]
                                                           ["(?!" ")"]])
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
   #"Look-behind group does not have an obvious maximum length"
   #"Range out of order in character class"])

(defn correct-parse-behavior?
  "Checks that our parser and re-pattern will either both successfully
  parse the string or will both throw an exception."
  [s]
  (let [[result err-msg] (try (re-pattern s)
                              [:parses]
                              (catch js/SyntaxError e
                                [:throws (.-message e)]))
        [our-result] (try (regexes/parse (str "/" s "/"))
                          [:parses]
                          (catch :default e
                            (if (= ::regexes/parse-error (:type (ex-data e)))
                              [:throws]
                              (throw e))))]
    (or (= result our-result)
        (and (= :throws result)
             (some #(re-find % err-msg) regex-parse-error-exceptions)))))

(deftest parser-regression
  (are [s] (correct-parse-behavior? s)
           ;; should always parse
           "{?" "[[]"
           ;; should never parse
           "?" "[(])" "[[](]" "[([])]"))

(defspec parser-spec (times 100)
  (prop/for-all [s gen-strings-that-might-be-regex-like]
    (do
      (correct-parse-behavior? s))))

(def gen-regex
  (let [maybes (gen/fmap #(try (re-pattern %)
                               (catch js/SyntaxError e))
                 gen-strings-that-might-be-regex-like)]
    (gen/such-that identity maybes 100)))

(def gen-generator-scenario
  (gen'/for [regex gen-regex
             :let [gen (try (regexes/gen-string-from-regex regex)
                            (catch :default e
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

(comment
  (parser-regression)

  (parse-spec)

  (cljs.test/run-tests)

  (clojure.test.check/quick-check 100
    (prop/for-all [s gen-strings-that-might-be-regex-like]
      (correct-parse-behavior? s)))

  (clojure.test.check/quick-check 100
    (prop/for-all [{:keys [regex s]} gen-generator-scenario]
      (re-matches regex s)))

  (try
    (regexes/parse "/[[](]/")
    (catch :default e
      (println "Error" (ex-data e))))

  (def email-re #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")

  (regexes/parse-raw "/[[]/")

  (gen/sample (regexes/gen-string-from-regex (re-pattern "[[]")))
  (try
    (gen/sample (regexes/gen-string-from-regex email-re))
    (catch :default e
      (println "Error" (ex-message e) (ex-data e))))
  )
