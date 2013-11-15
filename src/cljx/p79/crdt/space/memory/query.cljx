(ns p79.crdt.space.memory.query
  #+clj (:require [p79.crdt.space.memory.indexing :refer (index-comparator)])
  #+cljs (:require-macros [p79.crdt.space.memory.indexing :refer (index-comparator)])
  (:require [p79.crdt.space :as s :refer (->Tuple index)]
            [p79.crdt.space.types :refer (entity)]
            [clojure.set :as set]))

(defn- compare-values
  [x x2]
  (cond
    ;; TODO probably safe to make these comparisons identical?
    (= s/index-bottom x) -1
    (= s/index-bottom x2) 1
    (= s/index-top x) 1
    (= s/index-top x2) -1
    ;; all we're doing here is stratifying values of different types, then
    ;; ordering as with compare for each grouping
    ;; TODO we should be using sedan for all comparisons (whether in-memory or not?
    ;;   at least for the type comparisons, anyway)
    :else (let [t1 (type x)
                t2 (type x2)]
            (if (identical? t1 t2)
              (compare x x2)
              (compare (str t1) (str t2))))))

;; TODO Q: why does datomic have the indices that it has? Wouldn't one index
;; per "column" (time, tag, e, a, v) take care of all query possibilities?
;; (such an arrangement wouldn't yield covering indexes, for one)
;; (We probably never want to index on v, at least to start.  I don't want to 
;; think about 'schemas' yet...and, actually, indexing shouldn't be part of
;; the 'schema' anyway...

;; as long as each index is complete, we can just keep covering indexes as
;; sorted sets (with comparators corresponding to the different sorts)
(def index-types {[:e :a :v :write :remove] (index-comparator [:e :a :v :write :remove])
                  [:a :e :v :write :remove] (index-comparator [:a :e :v :write :remove])
                  [:a :v :e :write :remove] (index-comparator [:a :v :e :write :remove])
                  [:write :a :e :v :remove] (index-comparator [:write :a :e :v :remove])
                  [:v :a :e :write :remove] (index-comparator [:v :a :e :write :remove])})

(def available-indexes (-> index-types keys set))

(def empty-indexes (into {} (for [[index-keys comparator] index-types]
                              [index-keys (sorted-set-by comparator)])))

(defn match-tuple
  [match-vector]
  (apply ->Tuple (->> (repeat (- 5 (count match-vector)) '_)
                   (into match-vector))))

(defn- query-reorder-clauses
  [clauses]
  (sort-by (comp count #(filter '#{_} %) vals match-tuple) clauses))

(defn binding? [x]
  (and (symbol? x) (->> x name (re-matches #"\?.+"))))

(defn any? [x] (= '_ x))

(defn variable? [x] (or (binding? x) (any? x)))

(defn free-variables
  [clause]
  (set (filter variable? (map val (match-tuple clause)))))

(defn pairs
  [xs]
  (loop [pairs []
         [x & xs] xs]
    (if (empty? xs)
      pairs
      (recur (into pairs (map (fn [x2] [x x2]) xs)) xs))))

(defn pairs2
  [xs]
  (->> (iterate rest xs)
    (take-while seq)
    (mapcat #(when-let [x (and (next %) (first %))]
               (map (fn [y] [x y]) (rest %))))))

(defn- coerce-match-tuple*
  [x]
  (cond
   (variable? x) x
   :default (entity x)))

(defn coerce-match-tuple
  "Given a match tuple, returns a new one with bound values coerced appropriately
(e.g. values in entity position are turned into entity values, etc)."
  [t]
  (-> t
    (update-in [:e] coerce-match-tuple*)
    (update-in [:write] coerce-match-tuple*)))

; TODO eventually compile fns for each match-vector that use
; core.match for optimal filtering after index lookup

(defn sortable-match-tuple
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
        ;; TODO further, if we know at query-compile-time what indexes are
        ;; available (do we?), then we can warn/fail at that point, not here
        _ (when (nil? index)
            (throw (#+clj IllegalArgumentException. #+cljs js/Error.
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

(defn match
  [space previous-matches {:keys [index clause bindings whole-tuple-binding] :as clause-plan}]
  (->> (if (empty? previous-matches) #{{}} previous-matches)
    (map #(let [matches (match* space index (replace % clause) clause whole-tuple-binding)]
            (if (seq matches)
              (set/join previous-matches matches)
              #{})))
    (apply set/union)))

(defn query
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
                       (map #(query space (assoc q ::recur q :where %) results))
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
                    (->> (mapcat #(query space subquery %) args)
                      (map (apply juxt (:select subquery)))
                      (map #(zipmap (:destructuring clause-plan) %))
                      set
                      (set/join results)))))
    results
    (:where q)))
