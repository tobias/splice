(ns cemerick.splice.memory
  (:require [cemerick.splice :as s :refer (TupleStore)]
            [cemerick.splice.memory.query :as q]
            [cemerick.splice.hosty :refer (now)]
            [clojure.set :as set]))

(defn- add-tuples
  [indexes tuples]
  (reduce
    (fn [indexes index-keys]
      (update-in indexes [index-keys] into tuples))
    indexes
    (keys indexes)))

(deftype MemSpace [indexes as-of metadata]
  #+clj clojure.lang.IMeta
  #+clj (meta [this] metadata)
  #+cljs IMeta
  #+cljs (-meta [this] metadata)
  #+clj clojure.lang.IObj
  #+clj (withMeta [this meta] (MemSpace. indexes as-of meta))
  #+cljs IWithMeta
  #+cljs (-with-meta [this meta] (MemSpace. indexes as-of meta))
  TupleStore
  (available-indexes [this] (set (keys indexes)))
  (write* [this tuples]
    (MemSpace.
     (let [write (:write (first tuples))]
       ; TODO janky; need to make very clear the distinction between local
       ; writes and replicated writes
       ; TODO should local-only post-replication metadata (i.e. local wall-clock
       ; time of the application of a replicated write to the local set) go in
       ; its *own* write? Probably, lest we muck with tuple counts and
       ; signatures.
       (add-tuples indexes (cons (s/coerce-tuple write s/write-time (now) write) tuples)))
     as-of metadata))
  (scan [this index-spec beg end]
    (let [index (indexes index-spec)]
      (subseq index >= beg <= end))))

(defn in-memory
  ([] (MemSpace. q/empty-indexes nil {}))
  ([init-tuples] (MemSpace. (add-tuples q/empty-indexes init-tuples) nil {})))
