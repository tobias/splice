(ns p79.crdt.space.replication
  (:require [p79.crdt.space :as s :refer (q write* tuple->vector)]
            #+clj [p79.crdt.space.memory.planning :refer (plan)])
  #+cljs (:require-macros [p79.crdt.space.memory.planning :refer (plan)]))

(defn- maybe-notify-write
  [config change-fn watch-key space-ref old-space space]
  (change-fn space))

; TODO this is going to need to get asynchronous, fast
(defn watch-changes
  "Registers [fn] to be notified of writes to the space held in [space-ref]
(an atom, agent, ref, var, etc) that match the query specified in [config].
Returns the watch key used.

Each write is sent as a sequence of tuples."
  ([space-ref fn] (watch-changes space-ref nil fn))
  ([space-ref
    ; aping couchdb _changes args...
    {:keys [watch-key since limit heartbeat timeout #_filter]
     :or {}
     :as config}
    change-fn]
    (let [config (update-in config [:watch-key]
                            #(or % (keyword (gensym "watch-changes"))))]
      (remove-watch space-ref (:watch-key config))
      (add-watch space-ref (:watch-key config)
        (partial maybe-notify-write config change-fn))
      (:watch-key config))))

#+clj
(def replication-change-fn (comp
                             {clojure.lang.Atom swap!
                              clojure.lang.Agent send}
                             type))

#+cljs ; atoms are the only reference type available in cljs
(def replication-change-fn (fn [_] swap!))

(defn write-change
  [source-space]
  (when-let [write (-> source-space meta :p79.crdt.space/last-write)]
    ;; TODO it's absurd that a query is mixed up in the middle of replication
    (let [tuples (q source-space (plan {:select [?t]
                                        :args [?write]
                                        :where [[_ _ _ ?write :as ?t]]})
                   write)]
      ;; this :p79.crdt.space/last-write metadata isn't going to last long on this seq...
      (with-meta (apply concat tuples) {:p79.crdt.space/last-write write}))))

(defn write*-to-reference
  [target-space-reference write-tuples]
  ((replication-change-fn target-space-reference)
    target-space-reference write*
    (-> write-tuples meta :p79.crdt.space/last-write)
    (remove #(isa? (:a %) s/unreplicated) write-tuples)))

#+clj
(defn tuples->disk
  [path write-tuples]
  (with-open [w (clojure.java.io/writer path :append true)]
    (doseq [t (map tuple->vector write-tuples)]
      (.write w (pr-str t))
      (.write w "\n"))))
