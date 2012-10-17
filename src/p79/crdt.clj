(ns p79.crdt)

(defprotocol Joinable
  (join [a b]
    "Joins two CvRDTs, merging the provided states, potentially into a
new lower upper bound."))

(defprotocol CmRDT
  (update [this operation arguments]
    "Applies the given [operation] to a CmRDT using the supplied [arguments].
Returns a new value, the metadata of which may contain a set of operations
and arguments to propagate to other replicas of the CmRDT."))