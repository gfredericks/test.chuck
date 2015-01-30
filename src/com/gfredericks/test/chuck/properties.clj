(ns com.gfredericks.test.chuck.properties
  "Alternative to clojure.test.check.properties."
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.generators :as gen']))

(defmacro for-all
  "Alternative version of clojure.test.check.properties/for-all where
  the binding forms are interpreted as per
  com.gfredericks.test.chuck.generators/for."
  [bindings expr]
  `(gen/fmap

    ;; calling a private function because I don't know of a cleaner
    ;; way to do this :/
    (comp (#'prop/apply-gen #(%)) list)

    (gen'/for ~bindings (fn [] ~expr))))
