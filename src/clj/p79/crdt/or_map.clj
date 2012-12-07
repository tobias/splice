(ns p79.crdt.or-map
  (:require [p79.crdt :as crdt]
            [p79.crdt.map :as map]
            [p79.vclock :as vc]
            [clojure.set :as set]))

(defn- tag
  []
  (vc/entry))

;{K {tag V}}
;; this allows duplicate V entries added concurrently (with different tags)
;; TODO will need to be able to GC [tag V] entries for dupe values
;; Could be {K {V tag}}, but removes would be costly (need to search for tags)

(defn- sentinel=
  [x y]
  (= (if (set? x) x #{x})
     (if (set? y) y #{y})))

(declare join-strategies)

;; TODO will eventually dispatch on affected key as well
(defn join-strategy
  [or-map k]
  (-> or-map meta ::crdt/join-strategy (or :multi-value #_:lww)))

(deftype ObservedRemoveMap [entries metadata]
  Object
  (hashCode [this]
    (hash (seq this)))
  (equals [this other]
    (or (identical? this other)
        (and (map? other)
             (and (= (count this) (count other))
                  (every? (fn [[k v]] (sentinel= v (get this k))) other)))))
  
  clojure.lang.Seqable
  (seq [this] (map #(find this %) (keys entries)))
  
  clojure.lang.IPersistentCollection
  (count [this] (count (seq this)))
  (empty [this] (throw (UnsupportedOperationException.)))
  (equiv [this other] (.equals this other))
  (cons [this v] (throw (UnsupportedOperationException.)))
  
  clojure.lang.ILookup
  (valAt [this k] (when->> k (find this) val))
  (valAt [this k default] (or (get this k) default))
  clojure.lang.Associative
  (containsKey [this k] (contains? entries k))
  (entryAt [this k]
    (when-let [values (get entries k)]
      (clojure.lang.MapEntry. k
        ((-> this (join-strategy k) join-strategies :lookup) k values))))
  (assoc [this k v] (map/add this k v))
  clojure.lang.IPersistentMap
  (assocEx [this k v]
    (when (contains? this k) (throw (IllegalArgumentException. "key already present")))
    (map/add this k v))
  (without [this k]
    (if (contains? this k)
      (map/remove this k)
      this))
  
  java.io.Serializable
  clojure.lang.IFn
  (invoke [this k] (get this k))
  clojure.lang.IMeta
  (meta [this] metadata)
  clojure.lang.IObj
  (withMeta [this meta] (ObservedRemoveMap. entries meta)))

^:clj
(defmethod print-method ObservedRemoveMap
  [^ObservedRemoveMap o ^java.io.Writer w]
  (.write w "#")
  (.write w (.getName (class o)))
  (.write w "[")
  (print-method (.entries o) w)
  (.write w " ")
  (print-method (.metadata o) w)
  (.write w "]"))

;; This is seeming all a little (lot?) dodgy now...
(def join-strategies
  {:multi-value {:add (fn [this k v tags]
                        (ObservedRemoveMap.
                          (reduce
                            (fn [entries tag] (assoc-in entries [k tag] v))
                            (.entries this)
                            tags)
                          (.metadata this)))
                 :remove (fn [this k tags]
                           (let [tagvs ((.entries this) k)
                                 etags (select-keys tagvs tags)]
                             (ObservedRemoveMap.
                               (if (= tagvs etags)
                                 (dissoc (.entries this) k)
                                 (update-in (.entries this) (partial apply dissoc) tags))
                               (.metadata this))))
                 :lookup (fn [k value-entry]
                           (reduce
                             ;; entries can contain duplicate values for a given key if
                             ;; set concurrently with different tags. This merges the dupes
                             ;; on lookup, including the tags in a ::tags set in the value
                             ;; set's metadata
                             (fn [vset [tag v]]
                               (vary-meta (conj vset v)
                                          update-in [::tags] (fnil conj #{}) tag))
                             #{}
                             value-entry))}
   :lww {:add (fn [this k v tags]
                (let [latest-tag (max-key :t tags)
                      [_ {:keys [tag value]}] (find this k)]
                  (if (or (not tag)
                          (>= (:t latest-tag) (:t tag)))
                    (ObservedRemoveMap.
                      (assoc (.entries this) k {:tag latest-tag :value v})
                      (.metadata this))
                    this)))
         :remove (fn [this k tags]
                   (let [latest-tag (max-key :t tags)
                         [_ {:keys [tag value]}] (find this k)]
                     (if (and tag (not= tag (max-key :t (cons tag tags))))
                       (ObservedRemoveMap.
                         (dissoc (.entries this) k)
                         (.metadata this))
                       this)))
         :lookup #(:value %2)}})

(extend-type ObservedRemoveMap
  map/Map
  (add
    ([this k v]
      (if ((if (= :multi-value (join-strategy this k))
             sentinel=
             =)
            v (get this k))
        this
        (map/add this k v #{(tag)})))
    ([this k v tags]
      (let [map ((-> this (join-strategy k) join-strategies :add) this k v tags)]
        (if (identical? this map) this map))))
  (remove
    ([this k]
      (if-not (contains? this k)
        this
        (map/remove this k (-> ((.entries this) k) keys set))))
    ([this k tags]
      (let [map ((-> this (join-strategy k) join-strategies :remove) this k tags)]
        (if (identical? this map) this map))))
  (lookup [this k] (get this k)))

(defn create
  "Returns a new Observed-Remove Multimap containing the provided initial [keyvals]."
  [& keyvals]
  (ObservedRemoveMap.
    (into {} (for [[k v] (partition 2 keyvals)]
               [k {(tag) v}]))
    nil))

