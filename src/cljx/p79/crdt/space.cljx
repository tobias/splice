(ns p79.crdt.space
  (:require [p79.crdt.space.types :refer (entity)]
            [port79.hosty :refer (now current-time-ms)]
            [port79.uuid :refer (time-uuid random-uuid)]
            [clojure.set :as set]
            [clojure.walk :as walk]
            #+clj [clojure.pprint :as pp])
  ; this just forces the registration of data readers into
  ; cljs.tagged-literals/*cljs-data-readers* that wouldn't
  ; otherwise be available when compiling cljs statically
  #+cljs (:require-macros p79.crdt.space.types)
  (:refer-clojure :exclude (replicate)))

(def unreplicated ::unreplicated)
(def write-time ::write-time)
(derive write-time unreplicated)

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
  (throw (#+clj IllegalArgumentException. #+cljs js/Error. (apply str msg))))

(defn- seq->tuples
  [ls]
  (case (count ls)
    (4 5) [(apply coerce-tuple ls)]
    (throw-arg "Vector/list cannot be tuple-ized, bad size: " (count ls))))

(defn- map->tuples
  [m]
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

(extend-protocol AsTuples
  #+clj Object #+cljs default
  (as-tuples [x]
    (let [type (type x)
          extended? (cond
                      (sequential? x)
                      ; Clojure won't eval a symbol provided to `extend-type` et al.,
                      ; must be a list/seq TODO file an issue/patch for that
                      (extend-type #+clj (identity type) #+cljs type
                        AsTuples
                        (as-tuples [x] (seq->tuples x)))
                      (map? x)
                      (extend-type #+clj (identity type) #+cljs type
                        AsTuples
                        (as-tuples [x] (map->tuples x)))
                      ; extend-type returns different values in different languages (nil in Clojure)
                      :else false)]
      (if-not (false? extended?)
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
  ;; TODO a good idea, but not compatible with compile-time query planning
  ;(available-indexes [this])
  (index [this index-type])
  (q* [this query args]
     "Queries this space, returning a seq of results per the query's specification"))

(defn q
  [space {:keys [select planner args subs where] :as query} & arg-values]
  (q* space query arg-values))

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
