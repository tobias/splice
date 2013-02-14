(ns p79.crdt.space
  ^:clj (:require
          [p79.crdt.space.root-type :refer (defroottype)])
  ^:cljs (:require-macros
           [p79.crdt.space.root-type :refer (defroottype)])
  (:require [p79.crdt :as crdt]
            [p79.crdt.map :as map]
            [port79.hosty :refer (now current-time-ms)]
            [port79.uuid :refer (time-uuid random-uuid)]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.pprint :as pp])
  (:refer-clojure :exclude (replicate)))

(def unreplicated ::unreplicated)
(def write-time ::write-time)
(derive write-time unreplicated)

^:clj (defroottype :clj Entity entity "entity" e string?)
^:cljs (defroottype :cljs Entity entity "entity" e string?)
;; TODO not sure if this distinct reference type is worthwhile.
;; if the primary use case is referring to entities, then #entity "abcd" is
;; no worse than #ref #entity "abcd", right?  Even the generalized case of e.g.
;; #ref #s3 "https://..." doesn't provide much (any?) semantic benefit.
;(defroottype Ref reference "ref" e entity?)
;(defroottype Tag tag "tag" t entity?)

;; TODO don't quite like the e/a/v naming here
;; s/p/o is used in RDF, but might be too much of a tie to semweb
;; [element datum state]? :-P

(defn metadata? [tuple] (= (:e tuple) (:write tuple)))

(defprotocol AsTuples
  (as-tuples [x]))

(defrecord Tuple [e a v write remove]
  AsTuples
  (as-tuples [this] [this]))

(defn tuple? [x] (instance? Tuple x))

(defn coerce-tuple
  "Positional factory function for class p79.crdt.space.Tuple
that coerces any shortcut entity values to Entity instances.
This fn should therefore always be used in preference to the Tuple. ctor.
The 4-arg arity defaults [remove] to false."
  ([e a v write] (coerce-tuple e a v write false))
  ([e a v write remove]
    (Tuple. (entity e) a v (entity write) (entity remove))))

(let [tuple->vector* (juxt :e :a :v :write)]
  (defn tuple->vector
    [t]
    (let [v (tuple->vector* t)
          remove (:remove t)]
      (if remove (conj v remove) v))))

(defn throw-arg
  [& msg]
  (throw (^:clj IllegalArgumentException. ^:cljs js/Error. (apply str msg))))

(defn- seq->tuples
  [ls]
  (if (or (== 4 (count ls)) (== 5 (count ls)))
    [(apply coerce-tuple ls)]
    (throw-arg "Vector/list cannot be tuple-ized, bad size: " (count ls))))

(defn- map->tuples
  [m]
  ;; if Tuple ever loses its inline impl of as-tuples, we *must*
  ;; rewrite this to dynamically extend AsTuples to concrete Map
  ;; types; otherwise, there's no way to prefer an extension to
  ;; Tuple over one to java.util.Map
  (if-let [[_ e] (find m :db/id)]
    (let [time (:db/time m)
          s (seq (dissoc m :db/id))]
      (if s
        (mapcat (fn [[k v]]
                  (let [v (if (set? v) v #{v})]
                    (let [maps (filter map? v)
                          other (concat
                                  (remove map? v)
                                  (map (comp entity :db/id) maps))]
                      (concat
                        (mapcat map->tuples maps)
                        (map (fn [v] (coerce-tuple e k v nil nil)) other)))))
          s)
        (throw-arg "Empty Map cannot be tuple-ized.")))
    (throw-arg "Map cannot be tuple-ized, no :db/id")))

(extend-protocol AsTuples
  nil
  (as-tuples [x] []))

^:clj
(extend java.util.List
  AsTuples
  {:as-tuples seq->tuples})
^:clj
(extend java.util.Map
  AsTuples
  {:as-tuples map->tuples})

;; die, clojurescript, die
^:cljs
(extend-protocol AsTuples
  default
  (as-tuples [x]
    (let [type (type x)
          extended? (cond
                      (satisfies? ISequential x)
                      (extend-protocol AsTuples
                        type
                        (as-tuples [x] (seq->tuples x)))
                      (satisfies? IMap x)
                      (extend-protocol AsTuples
                        type
                        (as-tuples [x] (map->tuples x))))]
      (if extended?
        (as-tuples x)
        (throw-arg "No implementation of AsTuples available for " type)))))

(defprotocol Space
  (write* [this write-tag tuples] "Writes the given tuples to this space.")
  
  ;; don't expose until we know how to efficiently return indexes
  ;; that incorporate the ambient time filter
  ;; (can it be done, given that we need to keep existing index entries
  ;; "live" for replicated updates from the past?)
  #_
  (as-of [this] [this time]
         "Returns a new space restricted to tuples written prior to [time]."))

; TODO how to control policy around which writes need to be replicated?
; probably going to require query on the destination in order to determine workset
#_(defn update-write-meta
  [space written-tuples]
  (let [last-replicated-write (-> space meta ::replication :lwr)
        writes (set (map :write written-tuples))
        out-of-order-writes (when last-replicated-write
                              (set/union
                                (set/select #(neg? (compare % last-replicated-write)))
                                (or (-> space meta ::replication :ooow) #{})))]
    (vary-meta space merge
      {::writes writes
       ::replication {:lwr last-replicated-write
                      :ooow out-of-order-writes}})))

(defn update-write-meta
  [space write-tag]
  (vary-meta space assoc ::last-write write-tag))

(defn- add-write-tag
  [write tuples]
  (for [t tuples]
    (if (:write t) t (assoc t :write write))))

(defn write
  "Writes the given data to this space optionally along with tuples derived from
a map of operation metadata, first converting it to tuples with `as-tuples`."
  ([this data] (write this nil data))
  ([this op-meta data]
    (let [time (now)
          tuples (mapcat as-tuples data)
          _ (assert (not-any? :write tuples)
              (str "Data provided to `write` already has :write tag " (some :write tuples)))
          write (entity (time-uuid (.getTime time)))
          op-meta (merge {:time time} op-meta {:db/id write})
          tuples (->> tuples
                   (concat (as-tuples op-meta))
                   (add-write-tag write))]
      (-> this
        (write* write tuples)
        (update-write-meta write)))))

(defprotocol IndexedSpace
  (available-indexes [this])
  (index [this index-type])
  (q* [this query args]
     "Queries this space, returning a seq of results per the query's specification"))

(defn q
  [space {:keys [select planner args subs where] :as query} & arg-values]
  (q* space query arg-values))

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
    (let [config (update-in config [:watch-key] #(or % (keyword (gensym "watch-changes"))))]
      (remove-watch space-ref (:watch-key config))
      (add-watch space-ref (:watch-key config)
        (partial maybe-notify-write config change-fn))
      (:watch-key config))))

(def replication-change-fn (comp
                             ^:clj {clojure.lang.Atom swap!
                                    clojure.lang.Agent send}
                             ^:cljs {clojure.lang.Atom swap!}
                             type))

(defn write-change
  [source-space]
  (when-let [write (-> source-space meta ::last-write)]
    (let [tuples (q source-space '{:select [?t]
                                   :args [?write]
                                   :where [[_ _ _ ?write :as ?t]]}
                   write)]
      ;; this ::last-write metadata isn't going to last long on this seq...
      (with-meta (apply concat tuples) {::last-write write}))))

(defn write*-to-reference
  [target-space-reference write-tuples]
  ((replication-change-fn target-space-reference)
    target-space-reference write*
    (-> write-tuples meta ::last-write)
    (remove #(isa? (:a %) unreplicated) write-tuples)))

^:clj
(defn tuples->disk
  [path write-tuples]
  (with-open [w (clojure.java.io/writer path :append true)]
    (doseq [t (map tuple->vector write-tuples)]
      (.write w (pr-str t))
      (.write w "\n"))))

(deftype IndexBottom [])
(deftype IndexTop [])
(def index-bottom (IndexBottom.))
(def index-top (IndexTop.))

(defn assign-map-ids
  "Walks the provided collection, adding :db/id's to all maps that don't have one already."
  [m]
  (walk/postwalk
    (fn [x]
      (cond
        (not (map? x)) x
        (:db/id x) x
        :else (assoc x :db/id (random-uuid))))
    m))
