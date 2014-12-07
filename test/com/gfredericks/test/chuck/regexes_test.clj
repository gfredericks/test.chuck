(ns com.gfredericks.test.chuck.regexes-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.gfredericks.test.chuck.regexes :as regexes]
            [com.gfredericks.test.chuck.generators :as gen']
            [instaparse.core :as insta]))

(def gen-regex-parsing-attempt
  (gen'/for [s gen/string]
    (try (do
           (re-pattern s)
           [:parsed s])
         (catch java.util.regex.PatternSyntaxException e
           [:not-parsed s]))))

;; I don't think we can maintain this parse-or-doesn't-parse parity
;; because we can't distinguish #"[a-b]" from #"[b-a] in the parser.
(defspec the-parser-spec 1000
  (prop/for-all [[flag s] gen-regex-parsing-attempt]
    (try (let [x (regexes/parse s)]
           (case flag :parsed true :not-parsed false))
         (catch clojure.lang.ExceptionInfo e
           (if (= ::regexes/parse-error (:type (ex-data e)))
             (case flag :parsed false :not-parsed true)
             (throw e))))))
