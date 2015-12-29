(ns com.gfredericks.test.chuck.generators.experimental
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]))

(defn ^:private shrink-events
  [event-reduce-fn init-state new-state-handler ev-roses]
  (rose/fmap (fn [evs]
               (second
                (reduce (fn [[state out-evs] ev]
                          (let [decision (new-state-handler ev state)]
                            ;; (prn "DECISION IS" decision)
                            (case (first decision)
                              :keep
                              [(event-reduce-fn state ev) (conj out-evs ev)]

                              :fmap
                              (let [ev' ((second decision) ev)]
                                [(event-reduce-fn state ev') (conj out-evs ev')])

                              :discard
                              [state out-evs])))
                        [init-state []]
                        evs)))
             (rose/shrink vector ev-roses)))

(defn gen-events
  [event-reduce-fn init-state state->event-gen new-state-handler]
  ;; new-state-handler returns any of:
  ;; [:keep]
  ;; [:fmap func]
  ;; [:discard]
  (gen/gen-bind gen/nat
                (fn [ev-count-rose]
                  (let [ev-count (rose/root ev-count-rose)]
                    ;; this function returns a rose tree
                    (#'gen/make-gen
                     (fn [r size]
                       (loop [state init-state
                              ev-roses []
                              r r]
                         (if (= ev-count (count ev-roses))
                           (shrink-events event-reduce-fn
                                          init-state
                                          new-state-handler
                                          ev-roses)
                           (let [g (state->event-gen state)
                                 [r1 r2] (random/split r)
                                 ev-rose (gen/call-gen g r1 size)
                                 state' (event-reduce-fn state (rose/root ev-rose))]
                             (recur state' (conj ev-roses ev-rose) r2))))))))))

(defn naive-gen-events
  [event-reduce-fn init-state state->event-gen]
  (gen/bind gen/nat
            (fn [ev-count]
              ((fn self [state evs]
                 (if (= ev-count (count evs))
                   (gen/return evs)
                   (gen/bind (state->event-gen state)
                             (fn [ev]
                               (self (event-reduce-fn state ev) (conj evs ev))))))
               init-state []))))

(def init-state {:next-id 0
                 :people-by-id {}})

(def update-state nil)

(defmulti update-state
  (fn [state ev]
    (if (vector? ev)
      (first ev)
      (throw (ex-info "WTF" {:ev ev :state state})))))

(defmethod update-state :create
  [state [_ atts]]
  (-> state
      (update :next-id inc)
      (assoc-in [:people-by-id (:next-id state)]
                (assoc atts :id (:next-id state)))))

(defmethod update-state :update
  [state [_ id atts]]
  (-> state
      (update-in [:people-by-id id] merge atts)))

(defmethod update-state :delete
  [state [_ id]]
  (-> state
      (update :people-by-id dissoc id)))

(defn state->ev-gen
  [{:keys [people-by-id]}]
  (let [gen-create (gen/let [[age name] (gen/tuple gen/nat gen/string-ascii)]
                     [:create {:age age :name name}])]
    (if-let [ids (seq (keys people-by-id))]
      (gen/one-of [(gen/let [id (gen/elements ids)]
                     [:delete id])
                   (gen/let [[id atts]
                             (gen/tuple (gen/elements ids)
                                        (gen/let [[age name]
                                                  (gen/tuple gen/nat gen/string-ascii)
                                                  n (gen/choose 0 2)]
                                          (cond-> {}
                                            (<= n 1) (assoc :age age)
                                            (even? n) (assoc :name name))))]
                     [:update id atts])
                   gen-create])
      gen-create)))

(def naive-gen-people
  (naive-gen-events update-state
                    init-state
                    state->ev-gen))

(def gen-people
  (gen-events update-state
              init-state
              state->ev-gen
              (fn [ev new-state]
                (when-not (vector? ev)
                  (prn "WHAT" ev))
                (case (first ev)
                  :create
                  [:keep]

                  (:update :delete)
                  (let [id (second ev)]
                    (if (contains? (:people-by-id new-state) id)
                      [:keep]
                      (if-let [other-ids (seq (keys (:people-by-id new-state)))]
                        (let [new-id (->> other-ids
                                          (sort-by (fn [other-id]
                                                     (let [delta (- other-id id)]
                                                       [(Math/abs delta) delta])))
                                          (first))]
                          [:fmap (fn [ev]
                                   (assoc ev 1 new-id))])
                        [:discard])))))))

(comment

  (require 'clojure.test.check 'clojure.test.check.properties)

  (clojure.test.check/quick-check 500
   (clojure.test.check.properties/for-all [evs gen-people]
     (let [end-state (reduce update-state init-state evs)]
       (->> end-state :people-by-id vals (map :age) (not-any? #{42})))))
  )
