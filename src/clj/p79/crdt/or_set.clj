;; not used, result of early experimentation
(ns p79.crdt.or-set
  (:require [p79.crdt :as crdt]
            [p79.crdt.set :as cset]
            [p79.vclock :as vc]
            [clojure.set :as set]))

;; there's absolutely _no_ reason to be using a vclock as the tag here
(defn- tag
  []
  (vc/entry))

(deftype ObservedRemoveSet [adds removes metadata]
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
  
  ; only need to be instanceof Set to make equality work
  ; can't forsee a reason to implement more of the interface
  java.lang.Iterable
  (iterator [this] (.iterator (seq this)))
  
  clojure.lang.IPersistentSet
  (contains [this v]
    (boolean (seq (set/difference (adds v) (removes v)))))
  (get [this v]
    (and (contains? this v) v))
  (disjoin [this v] (throw (UnsupportedOperationException.)))
  
  clojure.lang.IPersistentCollection
  (count [this] (count (seq this)))
  (empty [this] (throw (UnsupportedOperationException.)))
  (equiv [this other] (.equals this other))
  (cons [this v] (throw (UnsupportedOperationException.)))
  
  java.io.Serializable
  clojure.lang.IFn
  (invoke [this k] (get this k))
  clojure.lang.IMeta
  (meta [this] metadata)
  clojure.lang.IObj
  (withMeta [this meta] (ObservedRemoveSet. adds removes meta)))

(defmethod print-method ObservedRemoveSet
  [^ObservedRemoveSet o ^java.io.Writer w]
  (.write w "#")
  (.write w (.getName (class o)))
  (.write w "[")
  (print-method (.adds o) w)
  (.write w " ")
  (print-method (.removes o) w)
  (.write w " ")
  (print-method (.metadata o) w)
  (.write w "]"))

(defn- as-set [xs] (if (set? xs) xs (set xs)))

(extend-type ObservedRemoveSet
  cset/Set
  (add
    ([this e]
      (if (contains? this e)
        this
        (cset/add this e #{(tag)})))
    ([this e tags]
      (ObservedRemoveSet.
        (update-in (.adds this) [e] (fnil set/union #{}) tags)
        (.removes this)
        (.metadata this))))
  (remove
    ([this e]
      (if-not (contains? this e)
        this
        (cset/remove this e ((.adds this) e))))
    ([this e tags]
      (let [etags (set/difference ((.adds this) e) tags)]
        (ObservedRemoveSet.
          (if (empty? etags)
            (dissoc (.adds this) e)
            (assoc (.adds this) e etags))
          (update-in (.removes this) [e] (fnil set/union #{}) tags)
          (.metadata this)))))
  (contains [this e] (.contains this e))
  
  p79.crdt/CvRDT
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
      (ObservedRemoveSet. adds {} (.metadata this)))))

(defn create
  "Returns a new Observed-Remove Set containing the provided initial values."
  [& vals]
  (let [adds (zipmap vals (map hash-set (repeatedly tag)))]
    (ObservedRemoveSet. adds {} nil)))

