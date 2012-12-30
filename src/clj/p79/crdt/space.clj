(ns p79.crdt.space
  (:require [p79.crdt :as crdt]
            [p79.crdt.map :as map]
            [cemerick.utc-dates :refer (now)]
            [clojure.contrib.graph :as g]
            [clojure.set :as set]
            [clojure.string :as str]
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
(defroottype Tag tag "tag" t reference?)

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

(def ^:private index-types #{[:e :a :v :tag] [:a :e :v :tag] [:a :v :e :tag] [:tag :a :e :v]
                             [:v :a :e :tag]})

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
            (fn [indexes index-keys]
              (update-in indexes [index-keys] (fnil index* empty-index) tuples index-keys))
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
                      (into match-vector))]
    (Tuple. e a v tag)))

(defn- query-reorder-clauses
  [clauses]
  (sort-by (comp count #(filter #{'_} %) vals match-tuple) clauses))

(defn- binding? [x]
  (and (symbol? x) (->> x name (re-matches #"\?.+"))))

(defn- any? [x] (= '_ x))

(defn- variable? [x] (or (binding? x) (any? x)))

(defn- pick-index
  [index-keys clause]
  (let [bound-keys (set (for [[k v] (match-tuple clause)
                              :when (and (not (= '_ v))
                                      (not (binding? v)))]
                          k))
        [prefix best-index] (->> index-keys
                              (map #(vector (count (take-while bound-keys %)) %))
                              (apply max-key first))]
    (when (pos? prefix)
      (with-meta best-index {:score prefix}))))

(defn- free-variables
  [clause]
  (set (filter variable? (map val (match-tuple clause)))))

(defn- clause-bindings
  "Same as `free-variables`, but excludes _"
  [clause]
  (let [variables (if (set? clause) clause (free-variables clause))]
    (set/select binding? variables)))

(defn- pairs
  [xs]
  (loop [pairs []
         [x & xs] xs]
    (if (empty? xs)
      pairs
      (recur (into pairs (map (fn [x2] [x x2]) xs)) xs))))

(defn- plan-where
  [db clauses]
  (reduce
    (fn [plan clause]
      (let [{prev-bound :bound-bindings prev-binds :bindings} (last plan)
            prev-bound (set/union prev-bound prev-binds)
            bound-clause (mapv #(if (get prev-bound %)
                                  :bound
                                  %) clause)
            bindings (clause-bindings clause)]
        (conj plan {:clause clause
                    :bound-clause bound-clause
                    ;; TODO warn when :index is nil!  Here or later?
                    :index (pick-index (available-indexes db) bound-clause)
                    :bindings bindings
                    :bound-bindings prev-bound})))
    []
    clauses))

; maintaining "user"-provided clause ordering, largely following the lead of
; https://groups.google.com/d/topic/datomic/6VkADvLx-QU/discussion
(defn- plan
  [db {:keys [select where as]}]
  (plan-where db where))

(defn match*
  [space index-keys match-vector]
  (let [index (index space index-keys)
        _ (when (nil? index)
            (throw (IllegalArgumentException. (str "No index available for " match-vector))))
        match-tuple (match-tuple match-vector)
        slot-bindings (filter (comp binding? val) match-tuple)
        match-vector (mapv (partial get match-tuple) index-keys)]
    (->> (subseq index
           >= (mapv #(if (variable? %) index-bottom %) match-vector)
           <= (mapv #(if (variable? %) index-top %) match-vector))
      (mapcat val)
      (map #(reduce (fn [match [tuple-key binding]]
                      (assoc match binding (tuple-key %)))
              {} slot-bindings))
      set)))

(defn match
  [space clauses]
  (let [plan (plan-where space clauses)]
    (reduce
      (fn [x {:keys [index clause bindings]}]
        (let [binding-limits (set/project x bindings)
              matches (->> (if (empty? binding-limits) #{{}} binding-limits)
                        (map #(match* space index (replace % clause)))
                        ;((fn [x] (println x) x))
                        (apply set/union))]
          (cond
            (and (seq matches) (seq x)) (set/join x matches)
            (seq matches) matches
            :else x)))
      nil
      plan)))

(defn query
  [space query]
  (let [matches (match space (:where query))]
    (map (apply juxt (:select query)) matches)))

(defn clause-graph
  "Returns a c.c.graph representing the relationships between the given clauses."
  [clauses]
  (reduce
    (fn [g [c c2 :as clauses]]
      (let [[b b2] (map clause-bindings clauses)]
        (if (or (= b b2) (not (some b b2)))
          g
          (-> (update-in g [:nodes] into [c c2])
            (update-in [:neighbors] (partial merge-with set/union)
              (if (set/subset? b b2) {c2 #{c}} {c #{c2}}))))))
    {:nodes #{} :neighbors {}}
    (pairs clauses)))

(comment
  (def g (clause-graph (map :clause p)))
  (pp/pprint g)
  (pp/pprint (g/dependency-list g)))