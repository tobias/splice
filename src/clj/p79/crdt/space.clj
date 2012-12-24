(ns p79.crdt.space
  (:require [p79.crdt :as crdt]
            [p79.crdt.map :as map]
            [cemerick.utc-dates :refer (now)]
            [clojure.set :as set]
            [clojure.pprint :as pp])
  (:refer-clojure :exclude (read)))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(comment
;         P
;     ___/|\___
;  br/    |    \span
;         | 
;       strong

[ts tag 1 :tag :p]
[ts tag 1 :ref/html 2]
[ts tag 1 :ref/html 3]
[ts tag 1 :ref/html 4]
[ts tag 2 :tag :br]
[ts tag 2 :rank 0.0]
[ts tag 3 :tag :span]
[ts tag 3 :rank 1.0]
[ts tag 4 :tag :strong]
[ts tag 4 :rank 0.5]
)

(defmacro defroottype
  [type-name ctor-name type-tag value-name value-pred]
  (let [value-field (symbol (str ".-" value-name))
        type-tag (str "#" type-tag " ")
        [arg arg2] [(gensym) (gensym)]
        [type-arg type-arg2] (map #(with-meta % {:tag type-name}) [arg arg2])]
    `(do
       (deftype ~type-name [~value-name]
         clojure.lang.IDeref
         (deref [~arg] (~value-field ~type-arg))
         Comparable
         (compareTo [~arg ~arg2]
           (compare (~value-field ~type-arg) (~value-field ~type-arg2)))
         Object
         (toString [~arg] (pr-str ~arg))
         (hashCode [~arg] (inc (hash (~value-field ~type-arg))))
         (equals [~arg ~arg2]
           (and (instance? ~type-name ~arg2)
             (= (~value-field ~type-arg) (~value-field ~type-arg2)))))
       (defmethod print-method ~type-name [~type-arg ^java.io.Writer w#]
         (.write w# ~type-tag)
         (print-method (~value-field ~type-arg) w#))
       (defmethod print-dup ~type-name [o# w#]
         (print-method o# w#))
       (#'pp/use-method pp/simple-dispatch ~type-name #'pp/pprint-simple-default)
       (defn ~(symbol (str ctor-name "?"))
         ~(str "Returns true iff the sole argument is a " type-name)
         [x#]
         (instance? ~type-name x#))
       (defn ~ctor-name
         ~(str "Creates a new " type-name)
         [e#]
         (when e#
           (cond
             (instance? ~type-name e#) e#
             (~value-pred e#) (~(symbol (str type-name ".")) e#)
             :else (throw
                     (IllegalArgumentException.
                       (str "Cannot create " ~type-name " with value of type "
                         (class e#))))))))))

(defroottype Entity entity "entity" e string?)
(defroottype Ref reference "ref" e entity?)
;(defroottype Tag tag "tag" t reference?)

(defn tombstone?
  [x]
  (and (vector? x) (== 2 (count x))
    (instance? Tag (first x))))

(defroottype Tombstone tombstone "tombstone" tag-value tombstone?)

;; TODO don't quite like the e/a/v naming here
;; s/p/o is used in RDF, but might be too much of a tie to semweb
;; [element datum state]? :-P

(defn metadata? [tuple] (not (:e tuple)))

(defprotocol AsTuples
  (as-tuples [x]))

(defrecord Tuple [e a v tag]
  AsTuples
  (as-tuples [this] [this]))

(defn ->Tuple
  "Positional factory function for class p79.crdt.space.Tuple
that coerces any shortcut tag and entity values to Tag and Entiy instances.
This fn should therefore always be used in preference to the Tuple. ctor."
  [e a v op-tag]
  (Tuple. (entity e) a v (tag op-tag)))

(extend-protocol AsTuples
  nil
  (as-tuples [x] [])
  java.util.List
  (as-tuples [ls]
    (if (== 4 (count ls))
      [(apply ->Tuple ls)]
      (throw (IllegalArgumentException.
               (str "Vector/list cannot be tuple-ized, bad size: " (count ls))))))
  java.util.Map
  (as-tuples [m]
    ;; if Tuple ever loses its inline impl of as-tuples, we *must*
    ;; rewrite this to dynamically extend AsTuples to concrete Map
    ;; types; otherwise, there's no way to prefer an extension to
    ;; Tuple over one to java.util.Map
    (if-let [[_ e] (find m :db/id)]
      (let [time (:db/time m)
            tag (:db/tag m)
            s (seq (dissoc m :db/id))]
        (if s
          (for [[k v] s]
            (->Tuple e k v tag))
          (throw (IllegalArgumentException. "Empty Map cannot be tuple-ized."))))
      (throw (IllegalArgumentException. "Map cannot be tuple-ized, no :db/id")))))

(defn- prep-tuples
  [op-tag tuples]
  (for [t tuples]
    (if (:tag t) t (assoc t :tag op-tag))))

(defprotocol Space
  (read [this]
     "Returns a lazy seq of the tuples in this space.")
  (write [this tuples] [this op-meta tuples]
     "Writes the given tuples to this space, optionally along with tuples derived from
a map of operation metadata.")
  (q [this query]
     "Queries this space, returning a seq of results per the query's specification")
  ;; don't expose until we know how to efficiently return indexes
  ;; that incorporate the ambient time filter
  ;; (can it be done, given that we need to keep existing index entries
  ;; "live" for replicated updates from the past?)
  #_
  (as-of [this] [this time]
         "Returns a new space restricted to tuples written prior to [time]."))

(defprotocol IndexedSpace
  (available-indexes [this])
  (index [this index-type]))

;; TODO Q: why does datomic have the indices that it has? Wouldn't one index
;; per "column" (time, tag, e, a, v) take care of all query possibilities?
;; (We probably never want to index on v, at least to start.  I don't want to 
;; think about 'schemas' yet...and, actually, indexing shouldn't be part of
;; the 'schema' anyway...

(deftype IndexBottom [])
(deftype IndexTop [])
(def ^:private index-bottom (IndexBottom.))
(def ^:private index-top (IndexTop.))

;; need [:aa _ _ _] to sort after [:a 5 #entity "foo" #ref #entity "bar"] 
(def index-comparator
  (reify java.util.Comparator
    (compare [this k k2]
      (or (->> (map #(cond
                       (= index-bottom %) -1
                       (= index-bottom %2) 1
                       (= index-top %) 1
                       (= index-top %2) -1
                       :else (let [type-compare (compare (str (type %)) (str (type %2)))]
                               (if (zero? type-compare)
                                 (compare % %2)
                                 type-compare)))
                 k k2)
            (remove #{0})
            first)
        0))))

(def empty-index (sorted-map-by index-comparator))

(defn index*
  "Returns a sorted-map of the distinct values of [keys] in the seq of
[maps] mapped to a set of those maps with the corresponding values of [keys]."
  ([maps keys] (index* empty-index maps keys))
  ([index maps keys]
    (when-not (sorted? index)
      (throw (IllegalArgumentException. "Cannot build index on unsorted map")))
    (let [values (apply juxt keys)
          conj-set (fnil conj #{})]
      (reduce
        #(update-in % [(values %2)] conj-set %2)
        index
        maps))))

(defn index-values
  [index]
  (apply concat (vals index)))

(def ^:private index-types
  (into {} (for [index-name [:eav :aev :ave :taev]]
             [index-name (->> (name index-name)
                           (map (comp keyword str))
                           (map #(if (= :t %) :tag %))
                           vec)])))

(deftype MemSpace [indexes as-of metadata]
  IndexedSpace
  (available-indexes [this] index-types)
  (index [this index-type] (indexes index-type))
  
  clojure.lang.IMeta
  (meta [this] metadata)
  clojure.lang.IObj
  (withMeta [this meta] (MemSpace. indexes as-of meta)))

(extend-type MemSpace
  Space
  (read [this]
    )
  (write
    ([this tuples] (write this nil tuples))
    ([this op-meta ts]
      (let [tag (entity (uuid))
            op-meta (merge {:time (now)} op-meta {:db/id tag})
            tuples (->> (mapcat as-tuples ts)
                     (concat (as-tuples op-meta))
                     (prep-tuples (reference tag)))]
        (MemSpace.
          (reduce
            (fn [indexes [index-type keys]]
              (update-in indexes [index-type] (fnil index* empty-index) tuples keys))
            (.-indexes this)
            index-types)
          (.-as-of this) (.-metadata this)))))
  
  #_
  (as-of
    ([this] as-of)
    ([this time]
      (MemSpace. (.-indexes this) time (.-metadata this)))))

(defn in-memory
  []
  (MemSpace. {} nil {}))

(defn- match-tuple
  [match-vector]
  (let [[e a v tag] (->> (repeat (- 4 (count match-vector)) '_)
                      (into match-vector)
                      (map #(if (list? %) '_ %)))]
    (Tuple. e a v tag)))

(defn query
  [space index-name match-vector]
  (let [match-tuple (match-tuple match-vector)
        index (index space index-name)
        index-keys (index-types index-name)
        match-vector (mapv (partial get match-tuple) index-keys)]
    (subseq index
      >= (mapv #(if (= '_ %) index-bottom %) match-vector)
      <= (mapv #(if (= '_ %) index-top %) match-vector))))
