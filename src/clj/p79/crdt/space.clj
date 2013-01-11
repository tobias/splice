(ns p79.crdt.space
  (:require [p79.crdt :as crdt]
            [p79.crdt.map :as map]
            [cemerick.utc-dates :refer (now)]
            [clojure.core.match :as match]
            [clojure.contrib.graph :as g]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [clojure.pprint :as pp])
  (:refer-clojure :exclude (read)))

(def ^:private rng (java.security.SecureRandom.))

(defn uuid [] (str (java.util.UUID/randomUUID)))
(defn time-uuid
  "Returns a sequential UUID. Guaranteed to:

(a) monotonically increase lexicographically
(b) contain [time] (or the current time in millis) as the most significant bits"
  ([] (time-uuid (System/currentTimeMillis)))
  ([time] (str (java.util.UUID. time (.nextLong rng)))))

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
;; TODO not sure if this distinct reference type is worthwhile.
;; if the primary use case is referring to entities, then #entity "abcd" is
;; no worse than #ref #entity "abcd", right?  Even the generalized case of e.g.
;; #ref #s3 "https://..." doesn't provide much (any?) semantic benefit.
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
          (mapcat (fn [[k v]]
                    (let [v (if (or (sequential? v) (set? v)) v #{v})]
                      (if (coll? v)
                        (let [maps (filter map? v)
                              other (concat
                                      (remove map? v)
                                      (map (comp reference entity :db/id) maps))]
                          (concat
                            (mapcat as-tuples maps)
                            (map (fn [v] (->Tuple e k v tag)) other))))))
            s)
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

(defn- compare-values
  [x x2]
  (cond
    (= index-bottom x) -1
    (= index-bottom x2) 1
    (= index-top x) 1
    (= index-top x2) -1
    :else (let [type-compare (compare (str (type x)) (str (type x2)))]
            (if (zero? type-compare)
              (compare x x2)
              type-compare))))

(defmacro ^:private index-comparator* [t t2 [k & tuple-keys]]
  (if-not k
    0
    `(let [x# (compare-values (~k ~t) (~k ~t2))]
       (if (zero? x#)
         (index-comparator* ~t ~t2 ~tuple-keys)
         x#))))

(defmacro ^:private index-comparator [tuple-keys]
  (let [t (gensym "t")
        t2 (gensym "t2")]
    `(with-meta
       (reify java.util.Comparator
         (compare [this# ~t ~t2]
           (index-comparator* ~t ~t2 ~tuple-keys)))
       {:index-keys '~tuple-keys})))

;; TODO as long as each index is complete, we can just keep covering indexes as
;; sorted sets (with comparators corresponding to the different sorts)
(def ^:private index-types {[:e :a :v :tag] (index-comparator [:e :a :v :tag])
                            [:a :e :v :tag] (index-comparator [:a :e :v :tag])
                            [:a :v :e :tag] (index-comparator [:a :v :e :tag])
                            [:tag :a :e :v] (index-comparator [:tag :a :e :v])
                            [:v :a :e :tag] (index-comparator [:v :a :e :tag])})

(def ^:private empty-indexes (into {} (for [[index-keys comparator] index-types]
                                        [index-keys (sorted-set-by comparator)])))

(deftype MemSpace [indexes as-of metadata]
  IndexedSpace
  (available-indexes [this] (-> index-types keys set))
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
      (let [time (now)
            tag (entity (time-uuid (.getTime time)))
            op-meta (merge {:time time} op-meta {:db/id tag})
            tuples (->> (mapcat as-tuples ts)
                     (concat (as-tuples op-meta))
                     (prep-tuples (reference tag)))]
        (MemSpace.
          (reduce
            (fn [indexes index-keys]
              (update-in indexes [index-keys] into tuples))
            (.-indexes this)
            (keys (.-indexes this)))
          (.-as-of this) (.-metadata this)))))
  
  #_
  (as-of
    ([this] as-of)
    ([this time]
      (MemSpace. (.-indexes this) time (.-metadata this)))))

(defn in-memory
  []
  (MemSpace. empty-indexes nil {}))

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

(defn- pairs
  [xs]
  (loop [pairs []
         [x & xs] xs]
    (if (empty? xs)
      pairs
      (recur (into pairs (map (fn [x2] [x x2]) xs)) xs))))

(defmulti plan #(-> %& second :planner))

(def ^:private predicate-clause? list?)
(defn- function-clause? [x]
  (and (vector? x) (list? (second x))))
(defn- expression-clause? [x] (or (predicate-clause? x) (function-clause? x)))

(defn- clause-bindings
  "Same as `free-variables`, but excludes _ and works with match vectors as
well as expression clauses."
  [clause]
  ; a little hokey about this function clause handling here
  (let [clause (if (function-clause? clause)
                 (second clause)
                 clause)]
    (->> (tree-seq coll? seq clause)
      (remove coll?)
      (filter binding?)
      set)))

; All :default planner does is maintain "user"-provided clause ordering,
; (excl. expression clauses), largely following the lead of
; https://groups.google.com/d/topic/datomic/6VkADvLx-QU/discussion
(defn- reorder-expression-clauses
  [clauses]
  (reverse
    (reduce
      (fn [clauses expr]
        (let [expr-bindings (clause-bindings expr)
              [after before] (split-with
                               #(empty? (set/intersection expr-bindings (clause-bindings %)))
                               clauses)]
          (concat after [expr] before)))
      (reverse (remove expression-clause? clauses))
      (filter expression-clause? clauses))))

(defn- compile-expression-clause
  [clause-bindings clause]
  (let [code `(~'fn [{:syms ~(vec clause-bindings)}]
                ~clause)]
    (with-meta (eval code)
      {:clause clause
       :code code})))

(declare plan-clause)

(defn- plan-clause*
  [db args bound-clause clause]
  (match/match [clause]
    
    [(_ :guard set?)]
    {:op :disjunction
     :clauses (if (every? vector? clause)
                (mapv
                  #(reduce (partial plan-clause db args)
                     [] (reorder-expression-clauses %))  
                  clause)
                (throw (IllegalArgumentException.
                         (str "Invalid disjunction; each sub-clause must be a vector (conjunction): "
                           clause))))}
    
    [(_ :guard list?)]
    {:predicate (compile-expression-clause (clause-bindings clause) clause)
     :op :predicate}
    
    [[destructuring (['q subquery-name & arguments] :seq :guard list?)]]
    {:op :subquery
     :destructuring destructuring
     :subquery subquery-name
     :args (vec arguments)}
    
    [[destructuring (['recur & arguments] :seq :guard list?)]]
    {:op :subquery
     :destructuring destructuring
     :subquery ::recur
     :args (vec arguments)}
    
    [[destructuring (fn-expr :guard list?)]]
    {:op :function
     :function (compile-expression-clause (clause-bindings fn-expr)
                 `(when-let [~destructuring ~fn-expr]
                    ~(let [bindings (if (symbol? destructuring)
                                      #{destructuring}
                                      (clause-bindings destructuring))]
                       #{(zipmap (map #(list 'quote %) bindings) bindings)})))}
    
    [([& _] :guard (partial every? (complement coll?)))]
    {:bound-clause bound-clause
     :index (pick-index (available-indexes db) bound-clause)
     :op :match}
    
    :else (throw (IllegalArgumentException. (str "Invalid clause: " clause)))))

(defn- plan-clause
  [db args plan clause]
  (let [{prev-bound :bound-bindings prev-binds :bindings} (-> plan meta :last-clause)
        prev-bound (set/union prev-bound prev-binds)
        bound-clause (mapv #(if (or (get prev-bound %) (some #{%} args))
                              :bound
                              %) clause)
        bindings (clause-bindings clause)
        planned-clause (merge {:clause clause
                               :bindings bindings
                               :bound-bindings prev-bound}
                         (plan-clause* db args bound-clause clause))]
    (conj
      (with-meta plan {:last-clause planned-clause})
      planned-clause)))

(defmethod plan :default
  [db {:keys [select args subs where] :as query}]
  (-> (assoc query
        :subs (into {} (for [[name subquery] subs]
                         [name (plan db subquery)]))
        :where (let [where (if (set? where) [where] where)]
                 (reduce (partial plan-clause db (:args query))
                   [] (reorder-expression-clauses where))))
    (vary-meta assoc :planned true)))

(defn- coerce-match-tuple
  "Given a match tuple, returns a new one with bound values coerced appropriately
(e.g. values in entity position are turned into entity values, etc)."
  [t]
  (-> t
    (update-in [:e] #(cond
                       (variable? %) %
                       (reference? %) (entity @%)
                       :default (entity %)))
    (update-in [:tag] #(cond
                         (variable? %) %
                         (entity? %) (reference %)
                         :else (reference (entity %))))))

; TODO eventually compile fns for each match-vector that use
; core.match for optimal filtering after index lookup

(defn- match*
  [space index-keys match-vector binding-vector]
  (let [index (index space index-keys)
        ;; TODO this should *warn*, not throw, and just do a full scan
        _ (when (nil? index)
            (throw (IllegalArgumentException.
                     (str "No index available for " match-vector))))
        binding-tuple (coerce-match-tuple (match-tuple binding-vector))
        match-tuple (coerce-match-tuple (match-tuple match-vector))
        slot-bindings (filter (comp binding? val) binding-tuple)
        sortable-match-tuple (fn [t wildcard]
                               (reduce (fn [t [k v]]
                                         (if-not (variable? v)
                                           t
                                           (assoc t k wildcard)))
                                 t t))]
    (->> (subseq index
           >= (sortable-match-tuple match-tuple index-bottom)
           <= (sortable-match-tuple match-tuple index-top))
      (filter (partial every? (fn [[k v]]
                                (let [v2 (k match-tuple)]
                                  (or (variable? v2) (= v v2))))))
      (map #(reduce (fn [match [tuple-key binding]]
                      (let [x (tuple-key %)]
                        (assoc match binding (if (reference? x) @x x))))
              {} slot-bindings))
      set)))

(defn- match
  [space previous-matches {:keys [index clause bindings] :as clause-plan}]
  (->> (if (empty? previous-matches) #{{}} previous-matches)
    (map #(let [matches (match* space index (replace % clause) clause)]
            (if (seq matches)
              (set/join previous-matches matches)
              #{})))
    (apply set/union)))

(declare query)

(defn- query*
  [space q results]
  (reduce
    (fn [results clause-plan]
      (case (:op clause-plan)
        :match (match space results clause-plan)
        :predicate (set/select (:predicate clause-plan) results)
        :function (let [function (:function clause-plan)]
                    (->> results
                      (map #(set/join #{%} (function %)))
                      (apply set/union)))
        :disjunction (->> (:clauses clause-plan)
                       (map #(query* space (assoc q ::recur q :where %) results))
                       (apply set/union))
        :subquery (let [subquery (if (= ::recur (:subquery clause-plan))
                                   (::recur q)
                                   (-> (:subs q)
                                     (get (:subquery clause-plan))
                                     (update-in [:subs] #(merge (:subs q) %))))
                        ; TODO we're losing the higher-level argument bindings here?
                        ; that may be a good thing, in terms of minimizing confusion around
                        ; naming/shadowing of arguments
                        args (map (fn [m] #{(into {} (map (fn [src dst] [dst (m src)])
                                                       (:args clause-plan) (:args subquery)))})
                               results)]
                    (->> (mapcat #(query* space subquery %) args)
                      (map (apply juxt (:select subquery)))
                      (map #(zipmap (:destructuring clause-plan) %))
                      set
                      (set/join results)))))
    results
    (:where q)))

(defn q
  [space {:keys [select planner args subs where] :as query} & arg-values]
  (let [query (if (-> query meta :planned) query (plan space query))
        args (zipmap args arg-values)
        matches (query* space query #{args})]
    (->> matches
      (map (apply juxt (:select query)))
      ;; TODO we can do this statically
      (remove (partial some nil?))
      set)))

(defn assign-map-ids
  [m]
  (walk/postwalk
    (fn [x]
      (cond
        (not (map? x)) x
        (:db/id x) x
        :else (assoc x :db/id (uuid))))
    m))

; this is probably outdated; something like this will be necessary to do sane
; query planning
(defn- clause-graph
  "Returns a c.c.graph representing the relationships between the given clauses."
  [clauses]
  (reduce
    (partial merge-with into)
    (for [[c c2] (pairs clauses)
          :let [[b b2] (map clause-bindings [c c2])
                overlap? (seq (set/intersection b b2))]]
      {:nodes #{c c2}
       :neighbors (cond
                    (not overlap?) {}
                    (and (list? c) (vector? c2)) {c #{c2}}
                    (and (list? c2) (vector? c)) {c2 #{c}}
                    (<= (count (set/difference b b2))
                      (count (set/difference b2 b))) {c2 #{c}}
                    :default {c #{c2}})})))
