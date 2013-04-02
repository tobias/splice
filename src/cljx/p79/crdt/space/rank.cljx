(ns p79.crdt.space.rank
  ^:clj (:require [p79.math :as math])
  ^:cljs (:require [p79.math :as math]
                   cljs.reader))

; This is a large N-way tree similar to the binary tree implementing a dense
; ordered set described in Shapiro et al.
; "CRDTs: Consistency without concurrency control" (http://arxiv.org/abs/0907.0929)
; that should be shallow enough to remain efficient for all known use cases

(defn- pad
  [x y]
  (list (concat x (repeat (- (count y) (count x)) 0))
        (concat y (repeat (- (count x) (count y)) 0))))

(defn- rank-compare
  [ns ns2]
  (or (->> (pad ns ns2)
        (apply map compare)
        (some #{1 -1}))
    0))

(deftype Rank [nums]
  ^:clj
  Comparable
  ^:clj
  (compareTo [this x] (rank-compare nums (.-nums ^Rank x)))
  ^:clj
  Object
  ^:clj
  (toString [x] (pr-str x))
  ^:clj
  (hashCode [this] (inc (hash nums)))
  ^:clj
  (equals [this x]
    (and (instance? Rank x)
      (= nums (.-nums ^Rank x))))
  ^:cljs
  IComparable
  ^:cljs
  (-compare [this x] (rank-compare nums (.-nums x)))
  ^:cljs
  IHash
  ^:cljs
  (-hash [this] (inc (hash nums)))
  ^:cljs
  IEquiv
  ^:cljs
  (-equiv [x y] (= nums (.-nums y)))
  ^:cljs
  IPrintWithWriter
  ^:cljs
  (-pr-writer [this w opts]
    (-write w "#p79.crdt.space.types.Rank")
    (-pr-writer [nums] w opts)))

(defn rank? [x] (instance? Rank x))
(defn- nums [rank] (.-nums ^Rank rank))

^:clj
(defmethod print-method Rank [^Rank x ^java.io.Writer w]
  (.write w (str "#" (.getName Rank)))
  (print-method [(.-nums x)] w))
^:clj
(defmethod print-dup Rank [o w] (print-method o w))
^:clj (require 'clojure.pprint)
^:clj
(#'clojure.pprint/use-method
  clojure.pprint/simple-dispatch
  Rank
  #'clojure.pprint/pprint-simple-default)

(defn rank
  [nums]
  (if (rank? nums) nums (Rank. nums)))

^:cljs
(cljs.reader/register-tag-parser! 'p79.crdt.space.types.Rank rank)

(defn- mean
  [x y]
  ; not just (/ (+ x y) 2.0) to minimize overflow
  (+ x (/ (- y x) 2.0)))

(def LOW [(- math/MAX)])
(def HIGH [math/MAX])

(defn between*
  [nx ny]
  (let [c (rank-compare nx ny)]
    (assert (not (zero? c)) "Cannot generate rank between equivalent ranks")
    (let [[nx ny] (if (== -1 c) [nx ny] [ny nx])]
      (loop [i 0]
        (let [x (nth nx i 0)
              y (nth ny i 0)]
          (if (= x y)
            (recur (inc i))
            (let [z (mean x y)]
              (if (or (== z x) (== z y) (== z math/INF) (== z math/-INF))
                (if (nth nx i nil)
                  (conj nx 1)
                  (into nx [0 1]))
                (conj (subvec nx 0 i) z)))))))))

(defn before* [x] (between* LOW x))
(defn after* [x] (between* x HIGH))

(def before (comp rank before* nums))
(def after (comp rank after* nums))

(defn between
  [rank1 rank2]
  (rank (between (nums rank1) (nums rank2))))

