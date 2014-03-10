(ns cemerick.splice.rank
  #+cljs (:require cljs.reader))

; This is an implementation of identifiers suitable for distinguishing values
; within a partially-ordered set.  Technically, strings representing locations
; in a wide N-way "tree" similar to the "treedoc" binary tree described in Letia
; et al.  "CRDTs: Consistency without concurrency control"
; (http://arxiv.org/abs/0907.0929) and Preguiça et al. "A commutative replicated
; data type for cooperative editing"
; (http://hal.inria.fr/docs/00/44/59/75/PDF/icdcs09-treedoc.pdf).
; Big difference is that this is a poset, not totally ordered; duplicate
; identifiers can exist.
; The big TODO TODO TODO is to provide a way to disambiguate identifiers, either
; via a given disambiguator (which might be known-unique), or (maybe more
; likely) probabilistically (i.e. choose midpoints randomly), since we probably
; will care more about preventing interleaving of contiguous insertions made by
; concurrent actors.

; These identifiers should be shallow/small enough to remain efficient for all
; known use cases, esp. if intermediate structure (DOM, etc) is in place.

; stuff copied from sedan; TODO get this and similar bits into a shared lib
(defn- char-code [^String s idx] (#+clj .codePointAt #+cljs .charCodeAt s idx))
(defn- char-code-seq [s] (map #(char-code s %) (range (count s))))
(defn sign
  [#+cljs x #+clj ^long x]
  (if (zero? x) 0 (/ x (Math/abs x))))

(deftype Rank [string]
  #+clj
  Comparable
  #+clj
  (compareTo [this x] (compare string (.-string ^Rank x)))
  #+clj
  Object
  #+clj
  (toString [x] string)
  #+clj
  (hashCode [this] (inc (hash string)))
  #+clj
  (equals [this x]
    (and (instance? Rank x)
      (= string (.-string ^Rank x))))
  #+cljs
  Object
  #+cljs
  (toString [this] string)
  #+cljs
  IComparable
  #+cljs
  (-compare [this x] (compare string (.-string x)))
  #+cljs
  IHash
  #+cljs
  (-hash [this] (inc (hash string)))
  #+cljs
  IEquiv
  #+cljs
  (-equiv [x y] (= string (.-string y)))
  #+cljs
  IPrintWithWriter
  #+cljs
  (-pr-writer [this w opts]
    (-write w "#cemerick.splice.rank.Rank")
    (-pr-writer [string] w opts)))

(defn rank? [x] (instance? Rank x))

#+clj
(defmethod print-method Rank [^Rank x ^java.io.Writer w]
  (.write w (str "#" (.getName Rank)))
  (print-method (vec (char-code-seq (.-string x))) w))
#+clj
(defmethod print-dup Rank [o w] (print-method o w))
#+clj (require 'clojure.pprint)
#+clj
(#'clojure.pprint/use-method
  clojure.pprint/simple-dispatch
  Rank
  #'clojure.pprint/pprint-simple-default)

(defn rank
  [string]
  (cond
   (rank? string) string
   (string? string) (Rank. string)
   (sequential? string) (Rank. (apply str (map char string)))
   :else (throw (ex-info (str "Cannot create Rank from value of type " (type string))
                         {:input string}))))

#+cljs
(cljs.reader/register-tag-parser! 'cemerick.splice.rank.Rank rank)

; could just as easily use a different range (topping out at 0xd7ff perhaps?)
; this keeps every component in each rank in the 2-byte UTF-8 range. Can be
; changed later at any time if testing determines a better top-line / default
; midpoint/origin.
(def low-code 0)
(def high-code 0x07ff)
(def LOW (str (char low-code)))
(def HIGH (str (char high-code)))

; Will need to instrument / repeat typical writing behaviour across many users /
; corpora in order to determine "optimal" starting point for new levels in the
; tree (perhaps repeat Letia's experiments w/ wikipedia changesets?).
(def ^:private new-branch-origin-code 0x400)
(def ^:private new-branch-origin-code-char (char new-branch-origin-code))

(defn- midpoint-prefix
  [prefix high-char-code]
  (if (<= (dec high-char-code) low-code)
    (str prefix (char low-code) new-branch-origin-code-char)
    (str prefix (char (long (/ high-char-code 2))))))

(defn- midpoint-diff
  [prefix low-suffix high-suffix]
  (let [ln (count low-suffix)
        hn (count high-suffix)]
    (loop [i 0]
      (cond
       (== i ln)
       (str prefix low-suffix (char low-code) new-branch-origin-code-char)
       (== i hn)
       (let [c (inc (char-code low-suffix i))]
         (if (== c high-code)
           (str prefix (subs low-suffix 0 i) HIGH new-branch-origin-code-char)
           (str prefix (subs low-suffix 0 i) (char c))))
       :else
       (let [lc (char-code low-suffix i)
             hc (char-code high-suffix i)
             diff (- hc lc)]
         (if (<= (Math/abs diff) 1)
           (recur (inc i))
           (str prefix (subs low-suffix 0 i) (char (+ lc (Math/abs (long (/ diff 2))))))))))))

(def origin* (str new-branch-origin-code-char))

; TODO pretty sure the body of this fn is close to being order-insensitive...
(defn between*
  [low high]
  (cond
   (or (= low "") (= high "")) (throw (ex-info "Empty string is an invalid endpoint" {}))

   ; only "need" this to prevent e.g. (after (rank [(inc high-code)]))
   (pos? (sign (compare low HIGH))) (throw (ex-info "Invalid rank, low above HIGH" {:low low}))
   (pos? (sign (compare high HIGH))) (throw (ex-info "Invalid rank, high above HIGH" {:high low}))

   (= high LOW) (throw (ex-info "Cannot generate rank that is less than LOW" {}))
   (= low HIGH) (throw (ex-info "Cannot generate rank that is greater than HIGH" {}))
   
   :else
   (let [ln (count low)
         hn (count high)
         len (min ln hn)]
     (loop [i 0]
       (if-not (== i len)
         (let [lc (char-code low i)
               hc (char-code high i)]
           (cond
            (== lc hc) (recur (inc i))
            (< lc hc) (midpoint-diff (subs low 0 i) (subs low i) (subs high i))
            :else (midpoint-diff (subs high 0 i) (subs high i) (subs low i))))
         (if (and (== len ln) (== len hn))
           (throw (ex-info "Cannot generate rank between equivalent strings"
                           {:low low :high high}))
           (if (== len ln)
             (midpoint-prefix low (char-code high i))
             (midpoint-prefix high (char-code low i)))))))))

(defn before*
  [s]
  (when (= s "") (throw (ex-info "Empty string is an invalid endpoint" {})))
  (let [lc-pos (dec (count s))
        lc (char-code s lc-pos)]
    (if (< (- lc 2) low-code)
      (between* LOW s)
      (str (subs s 0 lc-pos) (char (dec lc))))))

(defn after*
  [s]
  (when (= s "") (throw (ex-info "Empty string is an invalid endpoint" {})))
  (let [lc-pos (dec (count s))
        lc (char-code s lc-pos)]
    (if (> (+ lc 2) high-code)
      (between* s HIGH)
      (str (subs s 0 lc-pos) (char (inc lc))))))

(def origin (rank origin*))
(def low (rank LOW))
(def high (rank HIGH))

(defn before [^Rank rank] (Rank. (before* (.-string rank))))
(defn after [^Rank rank] (Rank. (after* (.-string rank))))
(defn between [^Rank a ^Rank b] (Rank. (between* (.-string a) (.-string b))))

