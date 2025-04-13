(ns com.gfredericks.test.chuck.generators.clj-time
  (:require [#?(:clj clj-time.core :cljs cljs-time.core) :as ct]
            [clojure.test.check.generators :as gen]))

(def valid-offset-fns [ct/millis ct/seconds ct/minutes ct/hours ct/days ct/months ct/years])

(def ^:private valid-offset-fn? (set valid-offset-fns))

(def ^:private yr-2000 (ct/date-time 2000))

(defn datetime
  "Generates datetime within given range and format.

   base-datetime => By default it'll calculate the dates from year 2000.
                    Generally this is a good idea instead of using (ct/now)
                    since giving the same seed will generate the same output.
                    If you would like to generate from a differnt base-datetime,
                    Pass for example (ct/now) to use current time,
                    Or pass a specific date-time (ct/date-time 2011 11 24)

   offset-min & offset-max => The offset number range
                              By default it is -1000 to 1000

   offset-fns => List of functions which will be used with the given offset.
                 It randomly picks one of the functions and
                 applies the random offset with the given range.
                 Check valid-offset-fns for possible values.
                 By default its all the values of valid-offset-fns.

   For example If you would like to generate datetime
   from last 10 months to next 10 months:
   (gen/sample (datetime {:offset-fns [clj-time.core/months]
                          :offset-min -10
                          :offset-max 10}))
   =>
   (#<DateTime 1999-11-01T00:00:00.000Z>
    #<DateTime 1999-12-01T00:00:00.000Z>
    #<DateTime 2000-05-01T00:00:00.000Z>
    ....)"
  ([]
   (datetime {}))
  ([{:keys [base-datetime offset-fns offset-min offset-max]
     :or {offset-fns valid-offset-fns
          offset-min -1000
          offset-max 1000
          base-datetime yr-2000}}]
   {:pre [(<= offset-min offset-max)
          (not-empty offset-fns)
          (every? valid-offset-fn?
                  offset-fns)]}
   (gen/fmap (fn [[offset-fn offset]]
               (->> offset
                    offset-fn
                    (ct/plus base-datetime)))
             (gen/tuple (gen/elements offset-fns)
                        (gen/large-integer* {:min offset-min
                                             :max offset-max})))))
