(ns com.gfredericks.test.chuck.clojure-test
  (:require [clojure.test.check :as tc]
            [clojure.test :refer :all]
            [clojure.test.check.properties :as prop]))

(defn report-when-failing [result]
  (is (:result result) result))

(defmacro capture-reports [body]
  `(let [reports# (atom [])]
     (with-redefs [report #(swap! reports# conj %)]
       ~@body)
     @reports#))

(defn pass? [reports]
  (every? #(= (:type %) :pass) reports))

(defn report-needed? [reports final-reports]
  (or (not (pass? reports)) (empty? final-reports)))

(defn save-to-final-reports [reports final-reports]
  (when (report-needed? reports @final-reports)
    (reset! final-reports reports)))

(defmacro checking [name tests bindings & body]
  `(testing ~name
     (let [final-reports# (atom [])]
       (report-when-failing (tc/quick-check ~tests
                              (prop/for-all ~bindings
                                (let [reports# (capture-reports ~body)]
                                  (save-to-final-reports reports# final-reports#)
                                  (pass? reports#)))))
       (doseq [r# @final-reports#]
         (report r#)))))
