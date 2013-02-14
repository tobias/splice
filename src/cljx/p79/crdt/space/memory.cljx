(ns p79.crdt.space.memory
  ^:clj (:require [clojure.core.match :as match])
  ^:cljs (:require-macros [clojure.core.match :as match])
  (:require [p79.crdt.space :as s :refer (Space IndexedSpace ->Tuple available-indexes
                                           entity index)]
            [port79.hosty :refer (now)]
            [clojure.math.combinatorics :refer (cartesian-product)]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.string :as str]))

(defn- compare-values
  [x x2]
  (cond
    (= s/index-bottom x) -1
    (= s/index-bottom x2) 1
    (= s/index-top x) 1
    (= s/index-top x2) -1
    :else (let [type-compare (compare (str (type x)) (str (type x2)))]
            (if (zero? type-compare)
              (compare x x2)
              type-compare))))

(defn- index-comparator* [t t2 tuple-keys]
  (if-let [k (first tuple-keys)]
    (let [x (compare-values (k t) (k t2))]
       (if (zero? x)
         (recur t t2 (rest tuple-keys))
         x))
    0))

(defn- index-comparator [tuple-keys]
  (with-meta
    ^:clj
    (reify java.util.Comparator
      (compare [this t t2]
        (index-comparator* t t2 tuple-keys)))
    ^:cljs #(index-comparator* % %2 tuple-keys)
    {:index-keys tuple-keys}))

;; TODO Q: why does datomic have the indices that it has? Wouldn't one index
;; per "column" (time, tag, e, a, v) take care of all query possibilities?
;; (such an arrangement wouldn't yield covering indexes, for one)
;; (We probably never want to index on v, at least to start.  I don't want to 
;; think about 'schemas' yet...and, actually, indexing shouldn't be part of
;; the 'schema' anyway...

;; as long as each index is complete, we can just keep covering indexes as
;; sorted sets (with comparators corresponding to the different sorts)
(def ^:private index-types {[:e :a :v :write :remove] (index-comparator [:e :a :v :write :remove])
                            [:a :e :v :write :remove] (index-comparator [:a :e :v :write :remove])
                            [:a :v :e :write :remove] (index-comparator [:a :v :e :write :remove])
                            [:write :a :e :v :remove] (index-comparator [:write :a :e :v :remove])
                            [:v :a :e :write :remove] (index-comparator [:v :a :e :write :remove])})

(def ^:private empty-indexes (into {} (for [[index-keys comparator] index-types]
                                        [index-keys (sorted-set-by comparator)])))

(defn- add-tuples
  [indexes tuples]
  (reduce
    (fn [indexes index-keys]
      (update-in indexes [index-keys] into tuples))
    indexes
    (keys indexes)))

(deftype MemSpace [indexes as-of metadata]
  ^:clj clojure.lang.IMeta
  ^:clj (meta [this] metadata)
  ^:cljs IMeta
  ^:cljs (meta [this] metadata)
  ^:clj clojure.lang.IObj
  ^:clj (withMeta [this meta] (MemSpace. indexes as-of meta))
  ^:cljs IWithMeta
  ^:cljs (-with-meta [this meta] (MemSpace. indexes as-of meta))
  Space
  (write* [this write-tag tuples]
    (let [tuples (cons (s/coerce-tuple write-tag s/write-time (now) write-tag) tuples)]
      (MemSpace.
        (add-tuples indexes tuples)
        as-of metadata)))
  #_
  (as-of
    ([this] as-of)
    ([this time]
      (MemSpace. (.-indexes this) time (.-metadata this)))))

(defn in-memory
  ([] (MemSpace. empty-indexes nil {}))
  ([init-tuples] (MemSpace. (add-tuples empty-indexes init-tuples) nil {})))

(defn- match-tuple
  [match-vector]
  (apply ->Tuple (->> (repeat (- 5 (count match-vector)) '_)
                   (into match-vector))))

(defn- query-reorder-clauses
  [clauses]
  (sort-by (comp count #(filter '#{_} %) vals match-tuple) clauses))

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

(defn- expand-disjunctive-clause
  [clause]
  (->> (map #(if (set? %) % #{%}) clause)
    (apply cartesian-product)
    (map (comp vector vec))
    set))

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
                (throw (^:clj IllegalArgumentException. ^:cljs js/Error.
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
    
    [[& _]]
    (let [disjunctions-expanded (expand-disjunctive-clause clause)]
      (if (< 1 (count disjunctions-expanded))
        (plan-clause* db args bound-clause disjunctions-expanded)
        (match/match [clause]
          
          [[_ _ _ _ ':as whole-tuple-binding]]
          (let [bound-clause (subvec bound-clause 0 4)]
            {:bound-clause bound-clause
             :index (pick-index (available-indexes db) bound-clause)
             :op :match
             :clause (subvec clause 0 4)
             :whole-tuple-binding whole-tuple-binding})
          
          :else {:bound-clause bound-clause
                 :index (pick-index (available-indexes db) bound-clause)
                 :op :match})))
    
    :else (throw (^:clj IllegalArgumentException. ^:cljs js/Error.
                   (str "Invalid clause: " clause)))))

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
                       :default (entity %)))
    (update-in [:write] #(cond
                           (variable? %) %
                           :else (entity %)))))

; TODO eventually compile fns for each match-vector that use
; core.match for optimal filtering after index lookup

(defn- sortable-match-tuple
  [t wildcard]
  (reduce (fn [t [k v]]
            (if-not (variable? v)
              t
              (assoc t k wildcard)))
    t t))

(defn match-index*
  [index match-tuple]
  (subseq index
    >= (sortable-match-tuple match-tuple s/index-bottom)
    <= (sortable-match-tuple match-tuple s/index-top)))

(defn match-index
  [index match-vector]
  (match-index* index (match-tuple match-vector)))

(defn- match*
  [space index-keys match-vector binding-vector whole-tuple-binding]
  (let [index (index space index-keys)
        ;; TODO this should *warn*, not throw, and just do a full scan
        _ (when (nil? index)
            (throw (^:clj IllegalArgumentException. ^:cljs js/Error.
                     (str "No index available for " match-vector))))
        binding-tuple (coerce-match-tuple (match-tuple binding-vector))
        match-tuple (coerce-match-tuple (match-tuple match-vector))
        slot-bindings (filter (comp binding? val) binding-tuple)]
    (->> (match-index* index match-tuple)
      (filter (partial every? (fn [[k v]]
                                (let [v2 (k match-tuple)]
                                  (or (variable? v2) (= v v2))))))
      ;; TODO need to be able to disable this filtering for when we want to 
      ;; match / join with :remove values
      (reduce
        (fn [s t]
          (if-let [r (:remove t)]
            (disj s (assoc t :write r :remove nil))
            (conj s t)))
        #{})
      (map #(reduce (fn [match [tuple-key binding]]
                      (assoc match binding (tuple-key %)))
              (if whole-tuple-binding
                {whole-tuple-binding %}
                {})
              slot-bindings))
      set)))

(defn- match
  [space previous-matches {:keys [index clause bindings whole-tuple-binding] :as clause-plan}]
  (->> (if (empty? previous-matches) #{{}} previous-matches)
    (map #(let [matches (match* space index (replace % clause) clause whole-tuple-binding)]
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

(extend-type MemSpace
  IndexedSpace
  (available-indexes [this] (-> index-types keys set))
  (index [this index-type] ((.-indexes this) index-type))
  (q* [space {:keys [select planner args subs where] :as query} arg-values]
    (let [query (if (-> query meta :planned) query (plan space query))
          args (zipmap args arg-values)
          matches (query* space query #{args})]
      (->> matches
        (map (apply juxt (:select query)))
        ;; TODO we can do this statically
        (remove (partial some nil?))
        set))))

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