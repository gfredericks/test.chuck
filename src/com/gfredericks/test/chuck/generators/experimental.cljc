(ns com.gfredericks.test.chuck.generators.experimental
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.rose-tree :as rose]))

(defn gen-events
  [event-reduce-fn init-state state->event-gen new-state-handler]
  ;; new-state-handler returns any of:
  ;; [:keep]
  ;; [:fmap func]
  ;; [:discard]
  (gen/gen-bind gen/nat
                (fn [ev-count-rose]
                  (let [ev-count (rose/root ev-count-rose)]
                    ((fn self [state evs-so-far]
                       )
                     init-state []))))
  )
