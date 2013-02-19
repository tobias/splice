(ns p79.crdt.space.memory.planning
  (:require [cemerick.cljs.macro :refer (defportable *cljs* *clj*)]
            [p79.crdt.space.memory.query :as query]
            [clojure.core.match :as match]
            [clojure.math.combinatorics :refer (cartesian-product)]
            [clojure.set :as set]
            [clojure.walk :as walk]))

(defmulti plan* :planner)

(def ^:private predicate-clause? list?)
(defn- function-clause? [x]
  (and (vector? x) (list? (second x))))
(defn- expression-clause? [x] (or (predicate-clause? x) (function-clause? x)))

(defn- pick-index
  [index-keys clause]
  (let [bound-keys (set (for [[k v] (query/match-tuple clause)
                              :when (and (not (= '_ v))
                                      (not (query/binding? v)))]
                          k))
        [prefix best-index] (->> index-keys
                              (map #(vector (count (take-while bound-keys %)) %))
                              (apply max-key first))]
    (when (pos? prefix)
      (with-meta best-index {:score prefix}))))

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
      (filter query/binding?)
      set)))

; this is probably outdated; something like this will be necessary to do more sophisticated
; query planning
(defn- clause-graph
  "Returns a c.c.graph representing the relationships between the given clauses."
  [clauses]
  (reduce
    (partial merge-with into)
    (for [[c c2] (query/pairs clauses)
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

(defn- expression-clause
  [clause-bindings clause]
  (let [code `(~'fn [{:syms ~(vec clause-bindings)}]
                ~clause)]
    (with-meta code {:clause (list 'quote clause) :code (list 'quote code)})))

(declare plan-clauses)

(defn- expand-disjunctive-clause
  [clause]
  (->> (map #(if (set? %) % #{%}) clause)
    (apply cartesian-product)
    (map (comp vector vec))
    set))

(defn- plan-clause*
  [args bound-clause clause]
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
      :subquery ::query/recur
      :args (vec arguments)}
    
    [[destructuring (fn-expr :guard list?)]]
    {:op :function
     :function (expression-clause (clause-bindings fn-expr)
                 `(when-let [~destructuring ~fn-expr]
                    ~(let [bindings (if (symbol? destructuring)
                                      #{destructuring}
                                      (clause-bindings destructuring))]
                       #{(zipmap (map #(list 'quote %) bindings) bindings)})))}
    
    [[& _]]
    (let [disjunctions-expanded (expand-disjunctive-clause clause)]
      (if (< 1 (count disjunctions-expanded))
        (plan-clause* args bound-clause disjunctions-expanded)
        (match/match [clause]
          
          [[_ _ _ _ ':as whole-tuple-binding]]
          (let [bound-clause (subvec bound-clause 0 4)]
            {:bound-clause bound-clause
             :index (pick-index query/available-indexes bound-clause)
             :op :match
             :clause (subvec clause 0 4)
             :whole-tuple-binding whole-tuple-binding})
          
          :else {:bound-clause bound-clause
                 :index (pick-index query/available-indexes bound-clause)
                 :op :match})))
    
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
                         (plan-clause* args bound-clause clause))]
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
       :else (quote-symbols %))
    #(if (symbol? %) (list 'quote %) %)
    query))

(defmacro plan
  [query]
  (quote-symbols (plan* query)))
