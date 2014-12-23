(ns com.gfredericks.test.chuck.regexes.charsets-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.generators :as gen']
            [com.gfredericks.test.chuck.regexes.charsets :as charsets]))

(def gen-simple-charset
  (gen/one-of
   [(gen/elements [charsets/all-ascii
                   charsets/line-terminators
                   charsets/all-unicode
                   charsets/all-unicode-but-line-terminators])
    (gen/elements (vals charsets/predefined-regex-classes))
    (gen'/for [i (gen/choose 0 (dec (charsets/size charsets/all-unicode)))]
      (charsets/singleton (charsets/nth charsets/all-unicode i)))]))

(def gen-charset
  (gen/recursive-gen
   (fn [g]
     (gen'/for [:parallel [op (gen/elements [charsets/union charsets/intersection
                                             charsets/difference])
                           cs1 g
                           cs2 g]]
       (op cs1 cs2)))
   gen-simple-charset))



(defspec union-is-commutative
  (prop/for-all [cs1 gen-charset
                 cs2 gen-charset]
    (= (charsets/union cs1 cs2)
       (charsets/union cs2 cs1))))
