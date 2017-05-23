(ns com.gfredericks.test.chuck.regexes.charsets
  "Sets of characters with efficient range representations.

  Includes large unicode characters, and so we can't use the
  Character class directly. Instead the API uses strings of
  one character (for small unicode characters) or two surrogates
  (for large unicode characters)."
  (:refer-clojure :exclude [empty nth range]))

(defn ^:private entry-size
  [[x1 x2]]
  (inc (- x2 x1)))

(defn ^:private entry-nth
  [[x1] idx]
  (+ x1 idx))

(defn char-string->long
  [s]
  (.charCodeAt s 0))

(defn long->char-string
  [x]
  {:pre [(<= 0 x 16r10FFFF)]}
  (js/String.fromCharCode x))

(defn ^:private compare-entries
  [[x1 x2] [x3 x4]]
  (if (< x2 x3) -1 (if (< x4 x1) 1 0)))

(def empty
  "The empty charset."
  (sorted-set-by compare-entries))

(defn singleton
  "Creates a charset from a single character string."
  [char-string]
  (let [x (char-string->long char-string)]
    (conj empty [x x])))

(defn range
  "Creates a charset from a range with an lower and upper bound, both
  inclusive."
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

(defn union*
  "Returns the union of the two charsets."
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

(defn union
  "Returns the union of the given character sets."
  [charset-1 & more]
  (reduce union* charset-1 more))

(defn difference
  "Returns a variant of the first charset without any of the
  characters in the second charset."
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
  "Returns the intersection of the two charsets."
  [charset-1 charset-2]
  ;; TODO: make a real implementation? meh probably not.
  (let [everything (union charset-1 charset-2)
        left (difference everything charset-2)
        right (difference everything charset-1)]
    (-> everything
        (difference left)
        (difference right))))

(defn size
  "Returns the size of the charset."
  [charset]
  (->> charset (map entry-size) (reduce +)))

(defn nth
  "Returns the character string from the charset at the given
  index, which must be (<= 0 idx (dec (size charset)))."
  [charset idx]
  (if (empty? charset)
    (throw (ex-info "Index out of bounds" {}))
    (loop [[x & xs] (seq charset)
           idx idx]
      (let [es (entry-size x)]
        (if (< idx es)
          (long->char-string (entry-nth x idx))
          (if xs
            (recur xs (- idx es))
            (throw (ex-info "Index out of bounds" {}))))))))


(defn ^:private singletons
  [& char-strings]
  (reduce union (map singleton char-strings)))

(def all-unicode
  "All unicode characters except (invalid) unpaired surrogates."
  (union (range "\u0000" "\uD7FF")
         (range "\uE000" "\uDBFF\uDFFF")))

(def all-ascii
  (range "\u0000" "\u007F"))

(def line-terminators
  (singletons "\n" "\r" "\u0085" "\u2028" "\u2029"))

(def single-character-line-breaks
  "This is everything that \\R matches, excepting the two character
  string \"\\r\\n\"."
  (union (range "\u000A" "\u000D")
         (singletons "\u0085" "\u2028" "\u2029")))

(def all-unicode-but-line-terminators
  (difference all-unicode line-terminators))

(def predefined-regex-classes
  (let [d (range "0" "9")
        s (singletons " " "\t" "\n" "\u000B" "\f" "\r")
        w (union d (range "a" "z") (range "A" "Z"))
        h (union (range "\u2000" "\u200a")
                 (singletons " " "\t" "\u00A0" "\u1680" "\u180e"
                             "\u202f" "\u205f" "\u3000"))
        v (singletons "\n" "\u000B" "\f" "\r" "\u0085" "\u2028" "\u2029")]
    {\d d, \s s, \w w, \h h, \v v
     \D (difference all-unicode d)
     \S (difference all-unicode s)
     \W (difference all-unicode w)
     \H (difference all-unicode h)
     \V (difference all-unicode v)}))
