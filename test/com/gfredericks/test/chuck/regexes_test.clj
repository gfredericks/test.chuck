(ns com.gfredericks.test.chuck.regexes-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [com.gfredericks.test.chuck.regexes :as regexes]
            [com.gfredericks.test.chuck.generators :as gen']
            [instaparse.core :as insta]))

(def gen-regex
  (gen'/for [s gen/string
             :let [re (try (re-pattern s)
                           (catch java.util.regex.PatternSyntaxException e e))]
             :when (instance? java.util.regex.Pattern re)]
    re))

;; test that anything re-pattern accepts can be parsed
(defspec the-parser-spec 1000
  (prop/for-all [re gen-regex]
    (let [parse (regexes/parse (str re))]
      (not (insta/failure? parse)))))
