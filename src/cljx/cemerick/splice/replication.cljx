(ns cemerick.splice.replication
  (:require [cemerick.splice :as s :refer (write* tuple->vector)]
            [cemerick.splice.memory :as mem]
            [cemerick.splice.memory.query :as q :refer (q)]
            #+clj [cemerick.splice.memory.planning :refer (plan)]
            #+clj [clojure.core.async :as async :refer (go go-loop >! <! alts!)]
            #+cljs [cljs.core.async :as async :refer (>! <!)]
            [clojure.string :as str])
  #+cljs (:require-macros [cemerick.splice.memory.planning :refer (plan)]
                          [cljs.core.async.macros :refer (go go-loop)]))

; might have gone overboard with the helper fns here...
(defn- named?
  "Returns true if [x] is 'named', i.e. a keyword or symbol."
  [x]
  #+clj (instance? clojure.lang.Named x) #+cljs (satisfies? INamed x))

(defn- attribute-of
  [x]
  (cond
    (named? x) x
    (s/tuple? x) (:a x)))

(defn- attribute-namespace
  [x]
  (some-> x attribute-of namespace (str/split #"\.")))

(defn- local-attribute?
  [x]
  (= "local" (first (attribute-namespace x))))

; TODO scanning the index directly is more efficient, but we need to be able to
; insert arbitrary queries to define the scope of replication. So, for all
; entities defined by the ACP query, find all writes that affect them >
; since-write, and then proceed with write eligibility from there
#_ ; TODO is there any reason to keep this code around anymore 
(defn- local-writes-since
  [space [site-id _ :as since-write]]
  (->> (s/scan space [:write :e :a :v :remove-write]
         (s/tuple s/index-bottom s/index-bottom s/index-bottom since-write)
         (s/tuple s/index-top s/index-top s/index-top [site-id s/index-top]))
       (drop-while #(= since-write (:write %)))
       (partition-by :write)))

; TODO this duplication is shameful, but not entirely clear how to resolve right this moment
(def ^:private writes-since-queries
  ; :all matches all tuples between since-write/top-write range - useful for
  ; driving actual replication
  ; :replicated matches only replication tuples within that range - useful for a
  ; local process reacting to changes replicated in from elsewhere
  {:all (plan {:select [?t ?rw]
                   :args [?since-write ?top-write]
                   :where [[_ _ _ (< ?since-write ?w ?top-write) ?rw :as ?t]]})
   :replicated (plan {:select [?t ?rw]
                   :args [?since-write ?top-write]
                   :where [[_ _ _ (< ?since-write ?w ?top-write) ?rw :as ?t]
                           [_ :local/replicated true ?w]]})})

(defn- writes-since
  [space [site-id _ :as since-write] query-type]
  (let [query (writes-since-queries query-type)]
    (assert query (str "Invalid query type for `writes-since`: " query-type))
    (->> (q space query since-write [site-id s/index-top])
     (map first)
     ; TODO will be unnecessary eventually, pending either explicit control of
     ; result ordering or implicit ordering based on scan range expression(s)
     (sort-by :write)
     (partition-by :write))))

(defn- writes-replicated-by
  "Returns a nested seq of tuples, partitioned by :write, that were replicated
into [space] from other sites based on the metadata added to those writes by the
'replication' write identified by [write-eid]."
  [space write-eid]
  (->> (q space (plan {:select [?t]
                   :args [?local-write]
                   :where [[?write :local/replicated true ?local-write]
                           [_ _ _ ?write ?include-removals :as ?t]]})
     write-eid)
    (apply concat)
    (partition-by :write)))

(defn- replication-eligible-writes
  "Starting from the given [write] (a seq of tuples), returns a seq of writes,
  each themselves a seq of tuples that are eligible for replication.  May return
  nil if no tuples from the given write are eligible for replication or none are
  linked to previously-replicated writes via `:local/replicated` attributes
  added to those replicated writes entites.  [write] is presumed to be a
  \"local\" write.  The specific rules for replication eligibility are described
  in the README."
  [space write]
  (let [attrs (set (map attribute-of write))
        write-eid (:write (first write))]
    (if (attrs :local/replicated)
      ; replication metadata write, replace with the replicated write
      (map #(vary-meta % assoc ::replication-write write-eid)
        (writes-replicated-by space (:write (first write))))
      (let [write (remove local-attribute? write)]
        (when-not (= #{["clock"]} (set (map attribute-namespace write)))
          [write])))))

(defn- replication-eligible-writes-since
  [space since-write type]
  (->> (writes-since space since-write type)
    (mapcat (partial replication-eligible-writes space))
    (remove nil?)))

(defn matching-write-channel
  "Returns a channel that yields writes (i.e. collections of tuples, each of
which constitutes a write) made to the space contained in the given [space-ref]
since [since-write] (which may be nil in order to yield all writes ever made to
the space.  Writes are put on the returned channel in the order they were
written to the source space.  The consumer of the returned write channel _must_
put a truthy value onto [control-channel] after consuming each write within 15
seconds in order to have the next write delivered, or the write channel will be
closed.  (This control semantic is deeply flawed, TODO will be revisited.)"
  [space-ref since-write control-channel write-class]
  (let [since-write (or since-write [(mem/site-idq @space-ref) s/index-bottom])
        write-channel (async/chan)
        watch-key (gensym (str "watch-" since-write))
        changes (async/chan (async/dropping-buffer 1))]
    ; ensure that we check for matching writes at least once right away
    (async/put! changes true)

    (add-watch space-ref watch-key
               (fn [watch-key space-ref old-space space]
                 (go (>! changes true))))

    ; TODO if the reader of the write channel goes away, these puts will block,
    ; and the watcher and any set of pending found writes will persist
    ; ~forever. Are timeouts the only mechanism we have to break this
    ; coordination problem?
    (go
      (loop [last-matching-write since-write
             writes nil]
        (if-let [w (first writes)]
          (do (>! write-channel w)
              (when-let [ctrl-val (first (alts! [control-channel (async/timeout 15000)]))]
                (recur (or (::replication-write (meta w)) (:write (first w)))
                  (rest writes)))) 
          (do (<! changes)
              (recur last-matching-write
                (replication-eligible-writes-since @space-ref last-matching-write write-class)))))

      (async/close! write-channel)
      (remove-watch space-ref watch-key))
    
    write-channel))

(defn peering-replication
  [src dest]
  (let [matching-write-control (async/chan)
        ; TODO find dest site-id, start from most recent in src from that site
        writes (matching-write-channel src nil matching-write-control :all)
        replication-control (async/chan)]
    (go-loop [last-write nil]
      (let [[v from-chan] (alts! [writes replication-control])]
        (if-not (coll? v)
          ; either a "cancel" signal via the control channel, or the matching-write-channel
          ; was closed
          (>! replication-control {:last-write last-write})
          ; TODO checking to see if the replicated write is in dest
          ; already or not stinks of non-idempotency; only relevant b/c
          ; of the local write containing replication-time
          (let [write-eid (:write (first v))]
            (when-not (seq (q/q @dest (plan {:select [?t]
                                             :args [?write]
                                             :where [[?write :clock/wall ?t]]})
                             write-eid))
              (swap! dest s/replicated-write v))
              (>! matching-write-control true)
              (recur write-eid)))))
    replication-control))

(comment (def a (atom (mem/in-memory)))
         (def b (atom (mem/in-memory)))
         (def c (atom (mem/in-memory)))
         (def ctrl (peering-replication a b))
         (def ctrl2 (peering-replication b c))
         (def rctrl (async/chan))
         (def replicated-changes (matching-write-channel b nil rctrl :replicated))
         (swap! a s/write [{::s/e "m" :x 0}])
         (swap! b s/write [{::s/e "m" :x "BBB"}])
         (swap! a s/write [["m" :x 0 nil (-> @a meta ::mem/last-write)]])
         (dotimes [x 20] (swap! a s/write [{::s/e "m" :x x}])))
