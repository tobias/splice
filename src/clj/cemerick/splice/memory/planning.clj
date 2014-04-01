(ns cemerick.splice.memory.planning
  (:require [cemerick.cljs.macro :refer (defportable *cljs* *clj*)]
            [cemerick.splice :as s]
            [cemerick.splice.memory.query :as q]
            [clojure.core.match :as match]
            [clojure.math.combinatorics :refer (cartesian-product)]
            [clojure.set :as set]
            [clojure.walk :as walk]))

(def ^:private ^:dynamic *&env*)

(defmulti plan* :planner)

(def ^:private predicate-clause? list?)

(defn- function-clause? [x]
  (and (vector? x) (list? (second x))))

(defn- scan-range-clause?
  [x]
  (and (vector? x)
       (boolean (some list? x))))

(defn- expression-clause?
  "Returns true for any clause that is or contains an \"expression\", which can
  depend upon bindings established in other clauses."
  [x]
  (or (predicate-clause? x)
      (function-clause? x))) 

(defn- pick-index
  [index-keys bound-clause original-clause]
  (let [bound-keys (set (for [[k v] (q/match-tuple bound-clause)
                              :when (and (not (= '_ v))
                                      (not (q/binding? v)))]
                          k))
        [prefix best-index] (->> index-keys
                              (map #(vector (count (take-while bound-keys %)) %))
                              (apply max-key first))]
    (if (pos? prefix)
      (with-meta best-index {:score prefix})
      (binding [*out* *err*]
        (println "WARNING: No index available for clause" original-clause)))))

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
      (filter q/binding?)
      set)))

; this is probably outdated; something like this will be necessary to do more sophisticated
; query planning
(defn- clause-graph
  "Returns a c.c.graph representing the relationships between the given clauses."
  [clauses]
  (reduce
    (partial merge-with into)
    (for [[c c2] (q/pairs clauses)
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

; All :default planner does is maintain "user"-provided clause ordering,
; (excl. "expression" clauses), largely following the lead of
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

; TODO this name is a misnomer; scan range clauses are also "expression" clauses
; insofar as they have binding dependencies, but they are not predicates or
; function invocations/expressions.
(defn- expression-clause
  [clause-bindings clause]
  (let [code `(~'fn [{:syms ~(vec clause-bindings)}]
                ~clause)]
    (with-meta code {:clause (list 'quote clause) :code (list 'quote code)})))

(defn- expand-disjunctive-clause
  [clause]
  (->> (map #(if (set? %) % #{%}) clause)
    (apply cartesian-product)
    (map (comp vector vec))
    set))

; TODO need to treat match clauses that contain scan range expressions as
; predicate clauses w.r.t. clause reordering
(defn- rewrite-scan-range-expr
  [bindings expr]
  (match/match expr
    ([(:or '> '>=) & components] :seq)
    ; TODO this is going to yield confusing error messages, since the
    ; expressions printed will be rewritten
    (rewrite-scan-range-expr
      bindings
      (list* ('{> < >= <=} (first expr))
        (reverse components)))

    ; TODO we should clamp these to the top/bottom of the
    ; corresponding sedan partition; needs to be done at runtime
    ; since we don't know the types of the bindings here.  So, we
    ; need some sigil indicating "top/bottom of the corresponding
    ; partition"
    ;; sometimes you want a clamp, sometimes not. Scans in :v, ok...
    ;; scans over :write, which can be of any type (at the moment?)
    ;; and e.g. replication queries need to traverse the entire space
    ;; up to index-top,
    ;; not so much
    ([(:or '< '<=) lower higher] :seq)
    (cond
      (and (every? q/binding? [lower higher])
        (not (bindings lower))
        (not (bindings higher)))
      (throw (IllegalArgumentException.
               (str "No free variable found in scan range expression:" expr)))
      
      ; (< ?a "foo")
      (and (q/binding? lower)
        (or (not (q/binding? higher))
          (bindings higher)))
      (list (first expr) q/bottom-symbol lower higher)

      ; (< "foo" ?a)
      (and (q/binding? higher)
        (or (not (q/binding? lower))
          (bindings lower)))
      (list (first expr) lower higher q/top-symbol)

      :else
      (throw (IllegalArgumentException.
               (str "More than one free variable found in scan range expression: " expr))))

    ([(:or '< '<=) bottom free top] :seq)
    (cond
      (contains? bindings free)
      (throw (IllegalArgumentException.
               (str "Middle member of scan range expression is bound: " expr)))

      (and (q/binding? free)
        (every? (fn [sym] (or (not (q/binding? sym))
                            (bindings sym)))
          [bottom top]))
      (apply list expr)

      :else
      (throw (IllegalArgumentException.
               (str "Invalid scan range expression: " expr))))

    (x :guard q/variable?)
    (list '<= q/bottom-symbol x q/top-symbol)

    :else
    (list '<= expr expr expr)))

(defn- match-spec
  [[op bottom binding top :as scan-range-expr]]
  {:pre [('#{< <=} op) bottom top]}
  (into {:inclusive (= op '<=)}
    (map vector [:bottom :binding :top] (rest scan-range-expr))))

(defn- match-spec? [x]
  (and (map? x) (contains? x :inclusive)))

(declare plan-clause* plan-clauses)

(defn- plan-match
  [args bindings bound-clause clause]
  (let [disjunctions-expanded (expand-disjunctive-clause clause)]
    (if (< 1 (count disjunctions-expanded))
      (plan-clause* args bindings bound-clause disjunctions-expanded)
      ; the only reason we're requiring a fully-specified tuple here is to avoid
      ; getting the ':as' and tuple binding name mixed up in the matching
      (let [whole-tuple-binding (match/match [clause]
                                  ; no pleasant way to check for suffix in match AFAIK
                                  [[_ _ _ _ ':as whole-tuple-binding]]
                                  whole-tuple-binding
                                  ; bound remove-write slot
                                  [[_ _ _ _ _ ':as whole-tuple-binding]]
                                  whole-tuple-binding
                                  :else nil)
            [clause bound-clause] (if whole-tuple-binding
                                    (map #(subvec % 0 (- (count %) 2)) [clause bound-clause])
                                    [clause bound-clause])
            _ (when (> (count (filter list? clause)) 1)
                (throw (IllegalArgumentException. (str "More than one scan expression in clause: "
                                                 clause))))
            match-specs (->> (q/extend-tuple-vector clause)
                             (mapv (partial rewrite-scan-range-expr bindings))
                             (mapv match-spec))]
        (merge (when whole-tuple-binding
                 {:whole-tuple-binding whole-tuple-binding})
               {:match-spec match-specs
                :op :match
                :index (pick-index q/available-indexes bound-clause clause)
                :clause clause
                :bound-clause bound-clause})))))

(defn- plan-clause*
  [args bindings bound-clause clause]
  (match/match [clause]
    
    [(_ :guard set?)]
    {:op :disjunction
     :clauses (if (every? vector? clause)
                (mapv
                  #(plan-clauses args (reorder-expression-clauses %))
                  clause)
                (throw (IllegalArgumentException.
                         (str "Invalid disjunction; each sub-clause must be a vector (conjunction): "
                           clause))))}
    
    [(_ :guard list?)]
    {:predicate (expression-clause (clause-bindings clause) clause)
     :op :predicate}
    
    [[destructuring (['q subquery-name & arguments] :seq :guard list?)]]
    {:op :subquery
     :destructuring destructuring
     :subquery subquery-name
     :args (vec arguments)}
    
    [[destructuring (['recur & arguments] :seq :guard list?)]]
    {:op :subquery
      :destructuring destructuring
      :subquery ::q/recur
      :args (vec arguments)}
    
    [(:or [(destructuring :guard #(or (map? %) (vector? %))) expr]
          [(destructuring :guard symbol?) (expr :guard list?)])]
    {:op :function
     :function (expression-clause (clause-bindings expr)
                 `(when-let [result# ~expr]
                    (try
                      (when-let [~destructuring result#]
                        ~(let [bindings (if (symbol? destructuring)
                                          #{destructuring}
                                          (clause-bindings destructuring))]
                           #{(zipmap (map #(list 'quote %) bindings) bindings)}))
                      (catch ~(if (:ns *&env*) :default 'Throwable) e#
                        ; coping with result not being destructurable
                        ; TODO maybe worth a runtime warning?
                        ))))}
    
    [[& _]]
    (plan-match args bindings bound-clause clause)
    
    :else (throw (IllegalArgumentException. (str "Invalid clause: " clause)))))

(defn- plan-clause
  [args {prev-bound :bound-bindings prev-binds :bindings} clause]
  (let [prev-bound (set/union prev-bound prev-binds)
        bound-clause (mapv #(if (or (get prev-bound %) (some #{%} args))
                              :bound
                              %) clause)
        bindings (clause-bindings clause)
        planned-clause (merge
                         {:clause clause
                          :bindings bindings}
                         (when prev-bound {:bound-bindings prev-bound})
                         (plan-clause* args (into (or prev-bound #{}) args)
                                       bound-clause clause))]
    planned-clause))

(defn- plan-clauses
  [args clauses]
  (reduce
    (let [previous-planned (atom nil)]
      #(let [planned (plan-clause args @previous-planned %2)]
         (reset! previous-planned planned)
         (conj % planned)))
    []
    clauses))

(defn- map-entry?
  [x]
  (or (instance? clojure.lang.MapEntry x)
    (and (vector? x) (== 2 (count x)))))

(defmethod plan* :default
  [{:keys [select planner args subs where] :as query}]
  (-> (assoc query 
        :subs (into {} (for [[name subquery] subs]
                         [name (plan* subquery)]))
        :where (let [where (if (set? where) [where] where)]
                 (plan-clauses (:args query) (reorder-expression-clauses where))))
    (vary-meta assoc :planned true)))

(defn quote-symbols
  [query]
  (walk/walk
    #(cond
       (and (map-entry? %) (#{:function :predicate} (first %))) %
       (and (map-entry? %) (#{:clause} (first %))) [(first %) (list 'quote (second %))]
       (list? %) (list 'quote %)
       :else (quote-symbols %))
    #(if (symbol? %) (list 'quote %) %)
    query))

(defmacro plan
  [query]
  (binding [*&env* &env]
    (quote-symbols (plan* query))))
