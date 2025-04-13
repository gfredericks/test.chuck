(ns com.gfredericks.test.chuck.generators.clj-time-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            #?@(:bb [] :clj [[clj-time.core :as ct]] :cljs [[cljs-time.core :as ct]])
            #?@(:bb [] :default [[com.gfredericks.test.chuck.generators.clj-time :as gen']])))

#?(:bb nil
:default
(defspec datetime-spec 100000
  (prop/for-all [dt (gen'/datetime {:offset-min 0
                                    :offset-max 100
                                    :offset-fns [ct/millis ct/seconds ct/minutes ct/hours ct/days ct/months]})]
    (ct/within? (ct/date-time 2000)
                (ct/date-time 2009)
                dt)))
)
