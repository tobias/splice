(ns cemerick.splice
  (:require [cemerick.splice.types :refer (entity)]
            [cemerick.splice.hosty :refer (now current-time-ms)]
            [cemerick.splice.uuid :refer (time-uuid random-uuid)]
            [clojure.set :as set]
            [clojure.walk :as walk]
            #+clj [clojure.pprint :as pp])
  ; this just forces the registration of data readers into
  ; cljs.tagged-literals/*cljs-data-readers* that wouldn't
  ; otherwise be available when compiling cljs statically
  #+cljs (:require-macros cemerick.splice.types)
  (:refer-clojure :exclude (replicate)))

(def unreplicated ::unreplicated)
(def write-time ::write-time)
(derive write-time unreplicated)

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

(defn coerce-tuple
  "Positional factory function for class cemerick.splice.Tuple that coerces any
implicit entity references to Entity instances.  This fn should therefore always
be used in preference to the Tuple. ctor."
  ([e a v write] (coerce-tuple e a v write nil))
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
  (if-let [[_ e] (find m :db/eid)]
    (let [s (seq (dissoc m :db/eid))]
      (if s
        (mapcat (fn [[k v]]
                  (let [v (if (set? v) v #{v})]
                    (let [maps (filter map? v)
                          other (concat
                                  (remove map? v)
                                  (map (comp entity :db/eid) maps))]
                      (concat
                        (mapcat map->tuples maps)
                        (map (fn [v] (coerce-tuple e k v nil nil)) other)))))
          s)
        (throw-arg "Empty Map cannot be tuple-ized.")))
    (throw-arg "Map cannot be tuple-ized, no :db/eid")))

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
                        (as-tuples [x] (apply coerce-tuple x)))
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

(defprotocol Space
  (write* [this write-tag tuples] "Writes the given tuples to this space."))

; TODO this ::last-write bullshit is useless
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
          op-meta (assoc op-meta :db/otime time :db/eid write)
          tuples (->> tuples
                   (concat (as-tuples op-meta))
                   (add-write-tag write))]
      (-> this
        (write* write tuples)
        (update-write-meta write)))))

; TODO is there ever any reason to distinguish between a Space and an
; IndexedSpace? What's the gd point if the tuplespace isn't indexed?

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
  "Walks the provided collection, adding :db/eid's to all maps that don't have one already."
  [m]
  (walk/postwalk
    (fn [x]
      (cond
        (not (map? x)) x
        (contains? x :db/eid) x
        :else (assoc x :db/eid (random-uuid))))
    m))
