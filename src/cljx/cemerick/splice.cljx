(ns cemerick.splice
  (:require [cemerick.splice.types :refer (entity)]
            [cemerick.splice.hosty :refer (now current-time-ms)]
            [cemerick.splice.uuid :refer (time-uuid random-uuid)]
            [cemerick.sedan :as sedan]
            [clojure.set :as set]
            [clojure.walk :as walk]
            #+clj [clojure.pprint :as pp])
  ; this just forces the registration of data readers into
  ; cljs.tagged-literals/*cljs-data-readers* that wouldn't
  ; otherwise be available when compiling cljs statically
  #+cljs (:require-macros cemerick.splice.types)
  (:refer-clojure :exclude (replicate)))

;; TODO this is stupid
(def unreplicated ::unreplicated)

(defn throw-arg
  [& msg]
  (throw (#+clj IllegalArgumentException. #+cljs js/Error. (apply str msg))))

(defprotocol AsTuples
  (as-tuples [x]))

; TODO should `e` be `eid`? It's not actually the _e_, it's an identifier.
(defrecord Tuple [e a v write remove-write]
  AsTuples
  (as-tuples [this] [this]))

(defn tuple? [x] (instance? Tuple x))

(defn tuple
  "Positional factory function for class cemerick.splice.Tuple that coerces any
implicit entity references to Entity instances.  This fn should therefore always
be used in preference to the Tuple. ctor."
  ([e a v] (tuple e a v nil nil))
  ([e a v write] (tuple e a v write nil))
  ([e a v write remove-write]
     (Tuple. (entity e) a v (entity write) (entity remove-write))))

(let [tuple->vector* (juxt :e :a :v :write)]
  (defn tuple->vector
    [t]
    (let [v (tuple->vector* t)
          remove (:remove-write t)]
      (if remove (conj v remove) v))))

(defn- map->tuples
  [m]
  (if-let [[_ e] (find m ::e)]
    (let [s (seq (dissoc m ::e))]
      (if s
        (mapcat (fn [[k v]]
                  (let [v (if (set? v) v #{v})]
                    (let [maps (filter map? v)
                          other (concat
                                  (remove map? v)
                                  (map (comp entity ::e) maps))]
                      (concat
                        (mapcat map->tuples maps)
                        (map (fn [v] (tuple e k v nil nil)) other)))))
          s)
        (throw-arg "Empty Map cannot be tuple-ized.")))
    (throw-arg "Map cannot be tuple-ized, no ::e")))

(extend-protocol AsTuples
  nil
  (as-tuples [x] [])

  #+clj Object #+cljs default
  (as-tuples [x]
    (let [type (type x)
          extended? (cond
                      (sequential? x)
                      ; Clojure won't eval a symbol provided to `extend-type` et al.,
                      ; must be a list/seq TODO file an issue/patch for that
                      (extend-type #+clj (identity type) #+cljs type
                        AsTuples
                        (as-tuples [x] [(apply tuple x)]))
                      (map? x)
                      (extend-type #+clj (identity type) #+cljs type
                        AsTuples
                        (as-tuples [x] (map->tuples x)))
                      ; extend-type returns different values in different
                      ; languages (nil in Clojure)
                      :else false)]
      (if-not (false? extended?)
        (as-tuples x)
        (throw-arg "No implementation of AsTuples available for " type)))))

(defprotocol TupleStore
  ; will always return the same set until query planning is no longer compile-time-only
  (available-indexes [this]
    "Returns a set of index 'specs' (vectors of keywords corresponding to tuple
     slots) representing the indexes that this TupleStore provides.")
  (write* [this tuples]
    "Writes the given tuples to the TupleStore.  It is assumed that the tuples
    constitute a single \"write\".")
  (scan [this index-spec beg end] [this index-spec beg end options]
    ; TODO would really like to support turning off inclusivity, but it's
    ; impractical given the way we're indexing right now. Could do it if the
    ; indexes weren't covering, but they are, and I don't know that it's worth
    ; it to change that just to avoid having to do runtime comparisons between
    ; lower/upper bounds at runtime for exclusive queries, see
    ; scan_range_queries.cljx tests
    ; :inclusive - defaults true, means that tuples matching [beg] or [end] will be
    ; included in the seq of returned tuples
    "Returns a (potentially lazy) seq of tuples that lie between the provided
[beg]inning and [end] tuples.  Ordering is controlled via the [options] map:

:reverse - defaults false; when true, tuples are returned in the opposite of
their \"natural order\" as per [index-spec].

Throws an exception if the requested [index-spec] is not available."))

(defn- add-write-tag
  [write tuples]
  (for [t tuples]
    (if (:write t) t (assoc t :write write)))) 

(defrecord Placeholder [temp-eid])
(defn peid [temp-eid] (Placeholder. temp-eid))
(defn placeholder? [p] (instance? Placeholder p))

(defn placeholders->eids
  "Replaces all placeholders with eids based on the [site-id] and
[last-write-num] of the replica to which the tuples will be written.  Equivalent
placeholders will be replaced with the same eid."
  [site-id last-write-num tuples]
  (let [eids (atom {})
        last-write-num (atom last-write-num)
        tuples (doall
                (map #(reduce
                        (fn [t slot]
                          (let [v (slot t)]
                            (if-not (placeholder? v)
                              t
                              (assoc t slot (or (@eids v)
                                                (let [eid [site-id (swap! last-write-num inc)]]
                                                  (swap! eids assoc v eid)
                                                  eid))))))
                        %
                        [:e :write])
                       tuples))]
    [@last-write-num tuples]))

(defn prep-write
  [op-meta data]
  (let [time (now)
        tuples (mapcat as-tuples data)
        _ (assert (not-any? :write tuples)
                  (str "Data provided to `write` already has :write tag " (some :write tuples)))
        write (peid (gensym "write"))
        op-meta (assoc op-meta :clock/wall time ::e write)
        tuples (->> tuples
                    (concat (as-tuples op-meta))
                    (add-write-tag write))]
    [write tuples]))

(defn write
  "Writes the given data to this space optionally along with tuples derived from
a map of operation metadata, first converting it to tuples with `as-tuples`."
  ([this data] (write this nil data))
  ([this op-meta data]
    (let [[write tuples] (prep-write op-meta data)]
      (write* this tuples))))

(defn- replicated-write-meta
  [write-eid]
  (let [time (now)]
    (->> [{::e write-eid :db/-repl-time time}
          {::e (peid 'write1) :clock/wall time}]
         (mapcat as-tuples)
         (add-write-tag (peid 'write1)))))

(defn replicated-write
  [this write-eid write-tuples]
  (write* this (concat write-tuples (replicated-write-meta write-eid))))

(def index-bottom sedan/bottom)
(def index-top sedan/top)

(defn assign-map-ids
  "Walks the provided collection, adding a :cemerick.splice/eid slot to all maps
that don't have one already."
  [m]
  (walk/postwalk
    (fn [x]
      (cond
        (not (map? x)) x
        (contains? x ::e) x
        :else (assoc x ::e (random-uuid))))
    m))
