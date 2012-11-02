(ns p79.crdt)

(defprotocol Joinable
  (join [a b]
    "Joins two CvRDTs, merging the provided states, potentially into a
new lower upper bound."))

(defprotocol CmRDT
  (update [this operation arguments]
    "Applies the given [operation] to a CmRDT using the supplied [arguments].
Returns a new value, the metadata of which may contain a set of operations
and arguments to propagate to other replicas of the CmRDT.")
  (snapshot [this t]
    "Returns a read-only data structure backed by this CmRDT containing its
states as of time [t] denominated in seconds."))

(defn log+
  [crdt & operations]
  (vary-meta crdt update-in [::downstream-log] (fnil into []) operations))

(defn log
  [crdt]
  (-> crdt meta ::downstream-log))

(defn truncate-log
  [crdt]
  (vary-meta crdt assoc ::downstream-log []))