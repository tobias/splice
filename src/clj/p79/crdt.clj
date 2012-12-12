(ns p79.crdt)

(defprotocol CvRDT
  (join [a b]
    "Joins two CvRDTs, merging the provided states, potentially into a
new lower upper bound."))

(defprotocol Snapshottable
  (snapshot [this t]
    "Returns a read-only data structure backed by this CRDT containing its
states as of time [t] denominated in seconds."))

; CmRDTs do not appear to be worth the hassle; there's always (?) a CvRDT underneath
; (in storage) that can be gradually replicated as necessary
#_
(defprotocol CmRDT
  (update [this operation arguments]
    "Applies the given [operation] to a CmRDT using the supplied [arguments].
Returns a new value, the metadata of which may contain a set of operations
and arguments to propagate to other replicas of the CmRDT."))

; stupid logging of CmRDT operations for later replication
#_#_#_
(defn log+
  [crdt & operations]
  (vary-meta crdt update-in [::downstream-log] (fnil into []) operations))

(defn log
  [crdt]
  (-> crdt meta ::downstream-log))

(defn truncate-log
  [crdt]
  (vary-meta crdt assoc ::downstream-log []))