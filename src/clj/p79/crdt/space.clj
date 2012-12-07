(ns p79.crdt.space
  (:require [p79.crdt :as crdt]
            [p79.crdt.map :as map]
            [cemerick.utc-dates :refer (now)]
            [clojure.set :as set])
  (:refer-clojure :exclude (remove)))

(defn uuid [] (str (java.util.UUID/randomUUID)))
(defn )

(comment
;         P
;     ___/|\___
;  br/    |    \span
;         | 
;       strong

[ts tag 1 :tag :p]
[ts tag 1 :ref/html 2]
[ts tag 1 :ref/html 3]
[ts tag 1 :ref/html 4]
[ts tag 2 :tag :br]
[ts tag 2 :rank 0.0]
[ts tag 3 :tag :span]
[ts tag 3 :rank 1.0]
[ts tag 4 :tag :strong]
[ts tag 4 :rank 0.5]
)




(defprotocol ITupleSpace
  (tuples [this]
     "Returns a lazy seq of the tuples in this space.")
  (q [this query]
     "Queries this space, returning a seq of results per the query's specification")
  (write [this tuples] [this tuples op-meta]
     "Writes the given tuples to this space, optionally along with tuples derived from
a map of operation metadata.")
  (as-of [this time]
         "Returns a new space restricted to tuples written prior to [time]."))

(defn $time [tuple] (nth tuple 0))
(defn $tag [tuple] (nth tuple 1))
(defn $e [tuple] (nth tuple 2))
(defn $a [tuple] (nth tuple 3))
(defn $v [tuple] (nth tuple 4))

(defn metadata? [tuple] (nil? ($e tuple)))
(defn has-time-tag? [tuple] (and (== 5 (count tuple))
                                 ($time tuple)
                                 ($tag tuple)))

(defn- prep-tuples
  ([tuples]
    (prep-tuples (now) (uuid) tuples))
  ([time tag tuples]
    (for [t tuples]
      (if (has-time-tag? t)
        t
        [time tag ($e t) ($a t) ($v t)]))))

(defn- map->tuples
  [m]
  )

(deftype TupleSpace [tuples as-of metadata]
  ITupleSpace
  (tuples [this e] (seq tuples))
  (write [this tuples] (write this tuples nil))
  (write [this ts op-meta]
    ; TODO op-meta tuples need same time/tag as 'actual' tuples
    (TupleSpace. (into tuples (map prep-tuples ts)) as-of metadata))
  (as-of [this time]
    (TupleSpace.
      (set/select #(<= 0 (compare ($time %) time)) tuples)
      as-of
      metadata))
  
  clojure.lang.IMeta
  (meta [this] metadata)
  clojure.lang.IObj
  (withMeta [this meta] (TupleSpace. tuples as-of meta)))

(defn in-memory
  []
  (TupleSpace. (sorted-set-by $time) (java.util.Date. 0) {}))
