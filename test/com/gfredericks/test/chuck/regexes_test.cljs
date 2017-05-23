(ns com.gfredericks.test.chuck.regexes-test
  (:require [cljs.test :refer-macros [deftest are is]]
            [com.gfredericks.test.chuck.regexes :as regexes]
            [clojure.test.check.generators :as gen]))

(deftest some-test
  (is (= 1 2)))

(println (pr-str (gen/sample gen/boolean 10)))
(println (pr-str (gen/sample (regexes/gen-string-from-regex "[a-z]") 10)))
