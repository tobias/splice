(ns cemerick.splice.memory.query
  #+clj (:require [cemerick.splice.memory.indexing :refer (index-comparator)])
  #+cljs (:require-macros [cemerick.splice.memory.indexing :refer (index-comparator)])
  (:require [cemerick.splice :as s :refer (->Tuple)]
            [cemerick.splice.types :as types]
            [cemerick.sedan :as sedan] ; used by index-comparator
            [cemerick.splice.walk :as walk]
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

(defn- echo [x]
  #+cljs (println x)
  #+clj (clojure.pprint/pprint x)
  x)

(defn extend-tuple-vector
  [v]
  (-> v
    (into (repeat (- 5 (count v)) '_))
    ; need subvec so that extra binding bits like `:as ?tuple` are trimmed off
    (subvec 0 5)))

(defn match-tuple
  [match-vector]
  (apply ->Tuple (extend-tuple-vector match-vector)))

(defn- query-reorder-clauses
  [clauses]
  (sort-by (comp count #(filter '#{_} %) vals match-tuple) clauses))

(defn binding? [x]
  (and (symbol? x) (->> x name (re-matches #"\?.+") boolean)))

(defn any? [x] (= '_ x))

(defn variable? [x] (or (binding? x) (any? x)))

(def ^:private predicate-clause? list?)

(defn- function-clause? [x]
  (and (vector? x)
    (list? (second x))
    (== 2 (count x))))

(defn- scan-range-clause?
  [x]
  (and (vector? x)
       (boolean (some list? x))))

(defn expression-clause?
  "Returns true for any clause that is or contains an \"expression\", which can
  depend upon bindings established in other clauses."
  [x]
  (or (predicate-clause? x)
      (function-clause? x))) 

(defprotocol IClauseTree
  (-branch? [x])
  (-children [x]))

(extend-protocol IClauseTree
  #+cljs default
  #+clj Object
  (-branch? [x] (coll? x))
  ; children is never called if branch? returns false, so defining it like so
  ; for non-collections is okay
  (-children [x] (seq x))
  cemerick.splice.types.Reference
  (-branch? [x] true)
  (-children [x]
    (if (types/unbounded? x)
      [@x]
      [@x (types/as-of x)]))
  cemerick.splice.types.OrderedAttribute
  (-branch? [x] true)
  (-children [x] [(.-attr x) (.-rank x)]))

(defn clause-seq
  "Returns a seq of all components of [clause], as per `tree-seq`, but with support for splice types."
  [clause]
  (tree-seq -branch? -children clause))

(defn clause-variables
  "Returns a set of the variables present in the given clause; includes _ as
well as named bindings."
  [clause]
  ; a little hokey about this function clause handling here
  (let [clause (if (function-clause? clause)
                 (second clause)
                 clause)
        variables (->> (clause-seq clause)
                    (remove coll?)
                    (filter variable?)
                    seq)]
    (when variables (set variables))))

(defn clause-bindings
  "Same as `free-variables`, but excludes _."
  [clause]
  (when-let [bindings (seq (filter binding? (clause-variables clause)))]
    (set bindings)))

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



(defprotocol IDestructurable
  (-destructure [binding-pattern value]))

(extend-protocol IDestructurable
  #+cljs default
  #+clj Object
  (-destructure [b v]
    ; only being able to extend protocols to concrete types in CLJS is a big PITA
    (if (and (vector? b) (vector? v))
      (apply merge (map -destructure b v))
      {}))

  #+cljs cljs.core.Symbol
  #+clj clojure.lang.Symbol
  (-destructure [b v]
    (if (binding? b)
      {b v}
      {}))

  cemerick.splice.types.Reference
  (-destructure [b v]
    (when (types/reference? v)
      (merge (-destructure @b @v)
        (-destructure (types/as-of b) (types/as-of v)))))

  cemerick.splice.types.OrderedAttribute
  (-destructure [b v]
    (when (types/oattr? v)
      (merge (-destructure (.-attr b) (.-attr v))
        (-destructure (.-rank b) (.-rank v))))))

(defprotocol IMatch
  (-matches? [query-value value]
    "Returns true if the actual value [v] matches the query value [qv]."))

(extend-protocol IMatch
  nil
  (-matches? [qv v] true)
  #+cljs default
  #+clj Object
  (-matches? [qv v] (= qv v))
  #+clj clojure.lang.Symbol
  #+cljs cljs.core.Symbol
  (-matches? [qv v]
    (or (variable? qv) (= qv v)))
  #+clj clojure.lang.IPersistentVector
  #+cljs cljs.core.PersistentVector
  (-matches? [qv v]
    (and (vector? v)
      (== (count qv) (count v))
      (every? identity (map -matches? qv v))))

  cemerick.splice.types.Reference
  (-matches? [qv v]
    (or
      ; supporting binding references to :e, :write, :remove-write
      ;; TODO this is kind of a hack, any better approach?
      (-matches? @qv v) 
      (and (types/reference? v)
        (-matches? @qv @v)
        (-matches? (types/as-of qv) (types/as-of v)))))
  cemerick.splice.types.OrderedAttribute
 (-matches? [qv v]
    (and (types/oattr? v)
      (-matches? (.-attr qv) (.-attr v))
      (-matches? (.-rank qv) (.-rank v)))))


; TODO eventually compile fns for each match-vector that use
; core.match for optimal filtering after index lookup

(defn- apply-removals
  [matching-tuples]
  (->> matching-tuples
    (reduce
      (fn [[ts rm] t]
        (if-let [r (:remove-write t)]
          [ts (conj rm (assoc t :write r :remove-write nil))]
          [(conj ts t) rm]))
      [#{} #{}])
    (#(apply set/difference %))))

; highlight matching chars
(defn- match*
  [space index-keys match-spec clause whole-tuple-binding]
  (when (and index-keys (not (contains? (s/available-indexes space) index-keys)))
    (throw (#+clj IllegalArgumentException. #+cljs js/Error.
                  (str "Planned index " index-keys " is not available in TupleSet"))))
  (let [match-tuple (match-tuple match-spec)
        slot-bindings (->> (map :binding match-spec)
                        (map list [:e :a :v :write :remove-write])
                        (reduce
                          (fn [slot-bindings [slot binding :as pair]]
                            (if (and binding (clause-bindings binding))
                              (conj slot-bindings pair)
                              slot-bindings))
                          []))
        bottom (apply ->Tuple (map :bottom match-spec))
        top (apply ->Tuple (map :top match-spec))
        remove-write-bound? (-> match-spec last :binding binding?)]
    (->> (s/scan space
           (or ((s/available-indexes space) index-keys)
             ; TODO probably should just use (first available-indexes)?
             [:e :a :v :write :remove-write])
           bottom
           top)
      ; the slowness of this filter is going to be hilarious
      (filter (partial every? (fn [[k v]]
                                (let [{qv :binding :keys [bottom top inclusive]} (k match-tuple)]
                                  (and (-matches? qv v)
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
      ; TODO datomic does this by forcing you to get a different "database" reference
      ; (Database.history()) to query history.  Current impl here implicitly
      ; turns on/off application of removal tuples based on the binding (or not)
      ; of the remove-write slot in scan range clauses.  TBD which is more
      ; capable / more convenient / more understandable.
      ; TODO should probably apply removals before checking applicability of query bindings
      (#(if remove-write-bound?
          %
          (apply-removals %)))
      (map #(reduce (fn [match [tuple-key binding]]
                      (merge match (-destructure binding (tuple-key %))))
              (if whole-tuple-binding
                {whole-tuple-binding %}
                {})
              slot-bindings))
      set)))

(extend-protocol walk/Walkable
  cemerick.splice.types.Reference
  (walkt [coll f]
    (types/reference (f @coll) (f (types/as-of coll))))
  cemerick.splice.types.OrderedAttribute
  (walkt [coll f]
    (types/oattr (f (.-attr coll)) (f (.-rank coll)))))

(defprotocol IBindable
  (-bind-to [v destination-slot]))

(extend-protocol IBindable
  #+cljs default
  #+clj Object
  (-bind-to [v slot] v)
  cemerick.splice.types.Reference
  (-bind-to [v slot]
    (case slot
      (:e :write :remove-write) @v
      v)))

(defn- fix-match-bindings
  [rel match-spec]
  (mapv (fn [{:keys [binding] :as spec} slot]
          (let [grounded-binding (walk/postwalk
                                   #(-bind-to (rel % %) slot)
                                   binding)]
            (if (not= binding grounded-binding)
              (assoc spec :bottom grounded-binding :top grounded-binding)
              (reduce
                (fn [spec slot]
                  (update-in spec [slot]
                    #(walk/postwalk
                       (fn [x]
                         (-bind-to (rel x x) slot))
                       %)))
                spec
                [:bottom :top]))))
    match-spec
    [:e :a :v :write :remove-write]))

; this alternative join operation exists for two reasons:
; (a) we need to control the equivalence test used when checking same-keyed
; values in relations, in order to ensure (= #ref "foo" "foo") (only when
; joining into :e, :write, and :write-remove, which is ensured by the impl of
; IBindable (TODO useless shitty name) for References.
; (b) clojure.set/join only works on sets; we want to be able to (eventually)
; provide query results as lazy seqs or channels delivering (distinct) relations
(defn- join*
  [x y]
  (cond
    (= x y) x
    (and (types/reference? x) (= @x y)) x
    (and (types/reference? y) (= @y x)) y
    :default (reduced nil)))

(defn join
  [xs ys]
  (->> (for [x xs
         y ys]
     (reduce
       (fn [n [k v]]
         (if-let [[_ v'] (find n k)]
           (let [x (join* v v')]
             (if (reduced? x)
               x
               (assoc n k x)))
           (assoc n k v)))
       x
       y))
    (remove nil?)
    set))

(defn match
  [space previous-matches {:keys [index clause match-spec whole-tuple-binding]}]
  (->> (if (empty? previous-matches) #{{}} previous-matches)
    (map #(let [#_#__ (when (seq previous-matches)
                    (println (fix-match-bindings (merge %
                                                  {bottom-symbol s/index-bottom
                                                   top-symbol s/index-top})
                              match-spec)))
                matches (match* space index
                          (fix-match-bindings (merge %
                                                {bottom-symbol s/index-bottom
                                                 top-symbol s/index-top})
                            match-spec)
                          clause whole-tuple-binding)]
            (if (seq matches)
              (join previous-matches matches)
              #{})))
    (apply set/union)))

(declare query)

(defn- query-clause
  [space q results clause-plan]
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

(defn query
  [space q results]
  (set/project
    (reduce
      (fn [results clause-plan]
        (query-clause space q results clause-plan))
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
        matches (query space q (if (seq args)
                                 #{(zipmap (:args q) arg-values)}
                                 #{{}}))]
    (map (apply juxt (:select q)) matches)))

