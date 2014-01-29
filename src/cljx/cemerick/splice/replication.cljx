(ns cemerick.splice.replication
  (:require [cemerick.splice :as s :refer (write* tuple->vector)]
            [cemerick.splice.memory :as mem]
            [cemerick.splice.memory.query :as q :refer (q)]
            #+clj [cemerick.splice.memory.planning :refer (plan)]
            #+clj [clojure.core.async :as async :refer (go go-loop >! <! alts!)]
            #+cljs [cljs.core.async :as async :refer (>! <!)])
  #+cljs (:require-macros [cemerick.splice.memory.planning :refer (plan)]
                          [cljs.core.async.macros :refer (go go-loop alts!)]))

; I want to write:
; (q space (plan {:select [?t]
;                 :args [?since]
;                 :where [[_ _ _ (> ?since ?w) :as ?t]]})
;    last-write)
; TODO need to support > >= < <= expressions in match clauses
; TODO how to characterize order? Implicit from the different range expressions
; in match clauses?

(defn- local-writes-since
  [space [site-id _ :as since-write]]
  (->> (s/scan space [:write :e :a :v :remove-write]
               (q/sortable-match-tuple
                (q/match-tuple ['_ '_ '_ since-write])
                s/index-bottom)
               (q/sortable-match-tuple
                (q/match-tuple ['_ '_ '_ [site-id s/index-top]])
                s/index-top))
       (drop-while #(= since-write (:write %)))
       (partition-by :write)))

(defn matching-write-channel
  "Returns a channel that yields writes (i.e. collections of tuples, each of
which constitutes a write) made to the space contained in the given [space-ref]
since [since-write] (which may be nil in order to yield all writes ever made to
the space.  Writes are put on the returned channel in the order they were
written to the source space.  The consumer of the returned write channel _must_
put a truthy value onto [control-channel] after consuming each write within 15
seconds in order to have the next write delivered, or the write channel will be
closed.  (This control semantic is deeply flawed, TODO will be revisited.)"
  [space-ref since-write control-channel]
  (let [since-write (or since-write [(mem/site-idq @space-ref) s/index-bottom])
        write-channel (async/chan)
        watch-key (gensym (str "watch-" since-write))
        changes (async/chan (async/dropping-buffer 1))]
    (add-watch space-ref watch-key
               (fn [watch-key space-ref old-space space]
                 (go (>! changes true))))
    ; TODO if the reader of the write channel goes away, these puts will block,
    ; and the watcher and any set of pending found writes will persist
    ; forever. Are timeouts the only mechanism we have to break this
    ; coordination problem?
    (go (loop [last-matching-write since-write
               writes (local-writes-since @space-ref since-write)]
          (if-let [w (first (seq writes))]
            (do (>! write-channel w)
                (when (first (alts! [control-channel (async/timeout 15000)]))
                  (recur (:write (first w)) (rest writes))))
            (when (<! changes)
              (recur last-matching-write (local-writes-since @space-ref last-matching-write)))))
        (async/close! write-channel)
        (remove-watch space-ref watch-key))
    write-channel))

(defn replicated-write
  [space tuples]
  )

(defn peering-replication
  [src dest]
  (let [ctrl (async/chan)
        ; TODO find dest site-id, start from most recent in src from that site
        writes (matching-write-channel src nil ctrl)]
    (go-loop [last-write nil]
             (let [w (<! writes)
                   write-eid (:write (first w))]
               (if-not w
                 last-write
                 ; TODO add replication-time metadata in new local write
                 ; TODO checking to see if the replicated write is in dest
                 ; already or not stinks of non-idempotency; only relevant b/c
                 ; of the local write containing replication-time
                 (do (when-not (seq (q/q @dest (plan {:select [?t]
                                                      :args [?write]
                                                      :where [[?write :db/otime ?t]]})
                                         write-eid))
                       (swap! dest s/write* w)) 
                     (>! ctrl true)
                     (recur write-eid)))))
    ctrl))
