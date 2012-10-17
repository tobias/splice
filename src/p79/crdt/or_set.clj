(ns p79.crdt.or-set
  (:require p79.crdt
            [clojure.set :as set]))

(defn- tag
  []
  (java.util.UUID/randomUUID))

(deftype ObservedRemoveSet [adds removes size metadata]
  clojure.lang.IPersistentCollection
  (count [this] size)
  (empty [this]
    (ObservedRemoveSet. {} {} 0 nil))
  (equiv [this other]
    (.equals this other))
  (cons [this v]
    (ObservedRemoveSet.
      (update-in adds [v] (fnil conj #{}) (tag))
      removes
      (inc size)
      metadata))
  
  Object
  (hashCode [this]
    (hash (set (seq this))))
  (equals [this other]
    (or (identical? this other)
        (and (set? other)
             (and (= (count this) (count other))
                  (every? #(contains? this %) (seq other))))))
  
  clojure.lang.Seqable
  (seq [this]
    (filter #(contains? this %) (keys adds)))
  ^:clj java.lang.Iterable
  ^:clj (iterator [this] (.iterator (seq this)))
  ; only need to be instanceof Set to make equality work
  ; can't forsee a reason to implement more of the interface
  ^:clj java.util.Set
  ^:clj (size [this] size)
  
  clojure.lang.IPersistentSet
  (contains [this v]
    (boolean (seq (set/difference (adds v) (removes v)))))
  (get [this v]
    (and (contains? this v) v))
  (disjoin [this v]
    (if-not (contains? this v)
      this
      (ObservedRemoveSet.
        (dissoc adds v)
        (update-in removes [v] (fnil into #{}) (adds v))
        (dec size)
        metadata)))
  
  java.io.Serializable
  clojure.lang.IFn
  (invoke [this k] (get this k))
  
  p79.crdt.Joinable
  (join [this other]
    (let [^ObservedRemoveSet other other
          adds (reduce
                 (fn [adds [v stale-tags]]
                   (let [tags (adds v)
                         pruned (set/difference tags stale-tags)]
                     (cond
                       (empty? pruned) (dissoc adds v)
                       (= tags pruned) adds
                       :default (assoc adds v pruned))))
                 (merge-with set/union (.adds this) (.adds other))
                 (merge-with set/union (.removes this) (.removes other)))]
      (ObservedRemoveSet. adds {} (count adds) metadata))))

^:clj
(defmethod print-method ObservedRemoveSet
  [^ObservedRemoveSet o ^java.io.Writer w]
  (.write w "#")
  (.write w (.getName (class o)))
  (.write w "[")
  (print-method (.adds o) w)
  (.write w " ")
  (print-method (.removes o) w)
  (.write w "]"))

(defn create
  "Returns a new Observed-Remove Set containing the provided initial values."
  [& vals]
  (let [adds (zipmap vals (map hash-set (repeatedly tag)))]
    (ObservedRemoveSet. adds {} (count adds) nil)))

