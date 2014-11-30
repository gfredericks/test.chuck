(ns com.gfredericks.test.chuck.regexes
  "Generic regex analysis code, not test.check specific."
  (:require [instaparse.core :as insta]))

(def grammar-path "com/gfredericks/test/chuck/regex.bnf")

(def parse
  (-> grammar-path
      (clojure.java.io/resource)
      (slurp)
      (insta/parser)))
