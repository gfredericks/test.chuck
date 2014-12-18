(ns com.gfredericks.test.chuck.regexes.charsets
  "Sets of characters with efficient range representations."
  (:refer-clojure :exclude [empty nth range])
  (:import [clojure.lang IPersistentVector]))

(defn ^:private entry-size
  [[x1 x2]]
  (inc (- x2 x1)))

(defn ^:private entry-nth
  [[x1] idx]
  (+ x1 idx))

(defn char-string->long
  [s]
  (cond (= 1 (count s))
        (long (first s))

        (= 2 (count s))
        (let [[c1 c2] s
              x1 (int c1)
              x2 (int c2)]
          (+ 16r10000
             (bit-shift-left (- x1 16rD800) 10)
             (- x2 16rDC00)))

        :else
        (throw (ex-info "Bad char-string!" {:arg s}))))

(defn long->char-string
  [x]
  {:pre [(<= 0 x 16r10FFFF)]}
  (if (< x 16r10000)
    (str (char x))
    (let [x' (- x 16r10000)
          high (bit-shift-right x' 10)
          low (bit-and x' 16r3FF)]
      (str (char (+ high 16rD800)) (char (+ low 16rDC00))))))

(defn ^:private compare-entries
  [[x1 x2] [x3 x4]]
  (if (< x2 x3) -1 (if (< x4 x1) 1 0)))

(def empty (sorted-set-by compare-entries))

(defn singleton
  [char-string]
  (let [x (char-string->long char-string)]
    (conj empty [x x])))

(defn range
  [char-string-1 char-string-2]
  (conj empty [(char-string->long char-string-1)
               (char-string->long char-string-2)]))

(defn ^:private merge-at
  [charset [x3 x4 :as entry]]
  (let [[[x1 x2 :as entry-left]] (rsubseq charset < entry)
        [[x5 x6 :as entry-right]] (subseq charset > entry)

        merge-left? (= x2 (dec x3))
        merge-right? (= x5 (inc x4))
        merged [(if merge-left? x1 x3)
                (if merge-right? x6 x4)]]

    (cond-> charset
            merge-left? (disj entry-left)
            merge-right? (disj entry-right)
            (or merge-left? merge-right?) (-> (disj entry) (conj merged)))))

(defn union
  [charset-1 charset-2]
  (if (< (count charset-1) (count charset-2))
    (recur charset-2 charset-1)
    (reduce (fn [cs [x1 x2 :as entry]]
              (let [;; can't easily use subseq for this, since it
                    ;; assumes at most one element is the cs is
                    ;; "equal" to the entry
                    overlaps (->> cs
                                  (drop-while #(neg? (compare-entries % entry)))
                                  (take-while #(zero? (compare-entries % entry))))

                    merged-with-overlaps
                    [(apply min x1 (map first overlaps))
                     (apply max x2 (map second overlaps))]]
                (-> (apply disj cs overlaps)
                    (conj merged-with-overlaps)
                    (merge-at merged-with-overlaps))))
            charset-1
            charset-2)))

(defn difference
  [charset-1 charset-2]
  (reduce (fn [cs [x1 x2 :as entry]]
            (let [overlaps (subseq cs >= entry <= entry)]
              (if (seq overlaps)
                (let [[x0] (first overlaps)
                      [_ x3] (last overlaps)]
                  (cond-> (apply disj cs overlaps)
                          (< x0 x1) (conj [x0 (dec x1)])
                          (> x3 x2) (conj [(inc x2) x3])))
                cs)))
          charset-1
          charset-2))

(defn intersection
  [charset-1 charset-2]
  ;; TODO: embetter?
  (let [everything (union charset-1 charset-2)
        left (difference everything charset-2)
        right (difference everything charset-1)]
    (-> everything
        (difference left)
        (difference right))))

(defn nth
  [charset idx]
  (if (empty? charset)
    (throw (IndexOutOfBoundsException.))
    (loop [[x & xs] (seq charset)
           idx idx]
      (let [es (entry-size x)]
        (if (< idx es)
          (long->char-string (entry-nth x idx))
          (if xs
            (recur xs (- idx es))
            (throw (IndexOutOfBoundsException.))))))))

(defn size [charset] (->> charset (map entry-size) (reduce +)))

(def all-unicode
  "All unicode characters except (invalid) unpaired surrogates."
  (union (range "\u0000" "\uD7FF")
         (range "\uE000" "\uDBFF\uDFFF")))

(def all-ascii
  (range "\u0000" "\u007F"))

(def line-terminators
  (->> ["\n" "\r" "\u0085" "\u2028" "\u2029"]
       (map singleton)
       (reduce union)))

(def all-unicode-but-line-terminators
  (difference all-unicode line-terminators))

(def predefined
  (let [d (range "0" "9")
        s (reduce union (map singleton [" " "\t" "\n" "\u000B" "\f" "\r"]))
        w (union d (union (range "a" "z")
                          (range "A" "Z")))]
    {\d d, \s s, \w w
     \D (difference all-unicode d)
     \S (difference all-unicode s)
     \W (difference all-unicode w)}))
