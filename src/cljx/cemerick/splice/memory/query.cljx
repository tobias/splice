(ns cemerick.splice.memory.query
  #+clj (:require [cemerick.splice.memory.indexing :refer (index-comparator)])
  #+cljs (:require-macros [cemerick.splice.memory.indexing :refer (index-comparator)])
  (:require [cemerick.splice :as s :refer (->Tuple)]
            [cemerick.splice.types :refer (entity)]
            [cemerick.sedan :as sedan] ; used by index-comparator
            [clojure.set :as set]))

;; as long as each index is complete, we can just keep covering indexes as
;; sorted sets (with comparators corresponding to the different sorts)
(def index-types {[:e :a :v :write :remove-write]
                  (index-comparator [:e :a :v :write :remove-write])
                  [:a :e :v :write :remove-write]
                  (index-comparator [:a :e :v :write :remove-write])
                  [:a :v :e :write :remove-write]
                  (index-comparator [:a :v :e :write :remove-write])
                  ; TODO this should only be available for reference values...?
                  [:v :a :e :write :remove-write]
                  (index-comparator [:v :a :e :write :remove-write])
                  ;; this index is (*maybe*) only useful for driving replication
                  ;; can never be used for "regular" queries, as its organization
                  ;; means that remove tuples are not colocated with the tuples
                  ;; they remove, so results can be incorrect
                  [:write :e :a :v :remove-write]
                  (index-comparator [:write :e :a :v :remove-write])})

(def available-indexes (-> index-types keys set))

(def empty-indexes (into {} (for [[index-keys comparator] index-types]
                              [index-keys (sorted-set-by comparator)])))

; necessary because we can't emit index-top and index-bottom values from the
; planner while it's exposed as a macro
(def bottom-symbol '⊥)
(def top-symbol '⊤)

(defn extend-tuple-vector
  [v]
  (into v (repeat (- 5 (count v)) '_)))

(defn match-tuple
  [match-vector]
  (apply ->Tuple (extend-tuple-vector match-vector)))

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

; TODO eventually compile fns for each match-vector that use
; core.match for optimal filtering after index lookup

; highlight matching chars
(defn- match*
  [space index-keys match-spec clause whole-tuple-binding]
  (when (and index-keys (not (contains? (s/available-indexes space) index-keys)))
    (throw (#+clj IllegalArgumentException. #+cljs js/Error.
                  (str "Planned index " index-keys " is not available in TupleSet"))))
  (let [match-tuple (match-tuple match-spec)
        slot-bindings (->> (map :binding match-spec)
                        (map list [:e :a :v :write :write-remove])
                        (reduce
                          (fn [slot-bindings [slot binding :as pair]]
                            (if (and binding (binding? binding))
                              (conj slot-bindings pair)
                              slot-bindings))
                          []))]
    (->> (s/scan space
           (or ((s/available-indexes space) index-keys)
             ; TODO probably should just use (first available-indexes)?
             [:e :a :v :write :remove-write])
           (apply ->Tuple (map :bottom match-spec))
           (apply ->Tuple (map :top match-spec)))
      ; the slowness of this filter is going to be hilarious
      (filter (partial every? (fn [[k v]]
                                (let [{v2 :binding :keys [bottom top inclusive]} (k match-tuple)]
                                  (and (or (nil? v2) (variable? v2) (= v v2))
                                    ; this is the only way to implement
                                    ; bounds-exclusive scans when the match
                                    ; clause has grounded values in it, e.g. [_
                                    ; :b (< 5 ?v)]; we pick the :aevw index, and
                                    ; so need to "manually" filter exclusive
                                    ; ranges. Certain clauses _don't_ need this
                                    ; (e.g. [_ _ _ (< ?since ?w) :as ?t]), but
                                    ; that's an optimization
                                    (or inclusive
                                      (and (neg? (sedan/compare bottom v))
                                        (neg? (sedan/compare v top)))))))))
      ;(#(do (prn %) %))
      ;; TODO need to be able to disable this filtering for when we want to 
      ;; match / join with :remove-write values
      (reduce
        (fn [[ts rm] t]
          (if-let [r (:remove-write t)]
            [ts (conj rm (assoc t :write r :remove-write nil))]
            [(conj ts t) rm]))
        [#{} #{}])
      ;(#(do (prn "xxx" slot-bindings %) %))
      (#(apply set/difference %))
      (map #(reduce (fn [match [tuple-key binding]]
                      (assoc match binding (tuple-key %)))
              (if whole-tuple-binding
                {whole-tuple-binding %}
                {})
              slot-bindings))
      set)))

(defn- fix-match-bindings
  [rel match-spec]
  (mapv (fn [spec]
          (if-let [[_ bval] (find rel (:binding spec))]
            (assoc spec :bottom bval :top bval)
            (reduce
              (fn [spec slot] (update-in spec [slot] #(rel % %)))
              spec
              [:bottom :binding :top])))
    match-spec))

(defn match
  [space previous-matches {:keys [index clause match-spec whole-tuple-binding]}]
  (->> (if (empty? previous-matches) #{{}} previous-matches)
    (map #(let [matches (match* space index
                          (fix-match-bindings (merge %
                                                {bottom-symbol s/index-bottom
                                                 top-symbol s/index-top})
                            match-spec)
                          clause whole-tuple-binding)]
            (if (seq matches)
              (set/join previous-matches matches)
              #{})))
    (apply set/union)))

(defn query
  [space q results]
  (set/project
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
                         ; self-recur should jump to the top of the current
                         ; query, not this dummy we're putting together to go down
                         ; each leg of the disjunction
                         ; TODO this means recur will fail outside of a
                         ; disjunctive clause; is that OK?
                         (map #(query space (assoc q ::recur-to q :where %) results))
                         (apply set/union))
          :subquery (let [subquery (if (= ::recur (:subquery clause-plan))
                                     (update-in (::recur-to q) [:lvl] (fnil inc 1))
                                     (-> (:subs q)
                                       (get (:subquery clause-plan))
                                       (update-in [:subs] #(merge (:subs q) %))))
                          ; TODO we're losing the higher-level argument bindings here?
                          ; that may be a good thing, in terms of minimizing confusion around
                          ; naming/shadowing of arguments
                          args (map #(hash-set (zipmap (:args subquery)
                                                 ((apply juxt (:args clause-plan)) %)))
                                 results)]
                      (if (seq args)
                        (->> (mapcat #(query space subquery %) args)
                          (map (apply juxt (:select subquery)))
                          (map #(zipmap (:destructuring clause-plan) %))
                          set
                          (set/join results))
                        results))))
      results
      (:where q))
    (:select q)))

;; TODO requiring query planning at compile-time is a sham. See bakery.md
#_
(defn- ensure-planned
  [query]
  (if (-> query meta :planned)
    query
    #+clj (eval (quote-symbols (plan* query)))
    #+cljs (throw (js/Error. "Cannot plan query at runtime in ClojureScript"))))

(defn q
  "Queries this space, returning a seq of results per the query's specification"
  [space {:keys [select planner args subs where] :as q} & arg-values]
  (let [;query (ensure-planned q)
        args (zipmap (:args q) arg-values)
        matches (query space q #{args})]
    (->> matches
         (map (apply juxt (:select q)))
         ;; TODO we can do this statically
         (remove (partial some nil?)))))

