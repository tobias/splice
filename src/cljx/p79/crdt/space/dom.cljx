(ns p79.crdt.space.dom
  (:require [p79.crdt.space :as s :refer (write q)]
            [p79.crdt.space.types :refer (entity)]
            [p79.crdt.space.rank :as rank]
            [p79.crdt.space.replication :as rep]
            [p79.crdt.space.memory :refer (in-memory)]
            [p79.crdt.space.memory.query :refer (index-types)]
            #+clj [p79.crdt.space.memory.planning :refer (plan)]
            #+cljs cemerick.cljs.dom
            #+cljs [goog.dom :as dom])
  #+cljs (:require-macros [p79.crdt.space.memory.planning :refer (plan)]
                          ; something jacked up in cljx that requires this?
                          [clojure.core :refer (some->)]))

(def local-replica (atom (in-memory)))
#_#_#_#_#_
(def ra (atom (in-memory)))
(def rb (atom (in-memory)))

(defn- configure-replication
  [src tgt]
  (rep/watch-changes src
    (comp (partial rep/write*-to-reference tgt) rep/write-change)))

(configure-replication ra local-replica)
(configure-replication rb local-replica)

(def html (s/assign-map-ids
            {:html/element :p
             
             :html/rank (rank/rank [])
             :html/children #{{:html/element :em
                               :html/rank (rank/rank (rank/before* []))
                               :html/attrs #{[:class "title"]
                                             [:id "name"]}                               
                               :html/children {:html/text "x"
                                               :html/rank (rank/rank [])}}
                              {:html/element :span
                               :html/attrs [:class "foo"]
                               :html/rank (rank/rank (rank/after* []))
                               :html/children {:html/text "y"
                                               :html/rank (rank/rank [])}}}}))

#_#_#_#_#_#_
(swap! ra s/write [html])

(q @local-replica
  (plan {:select [?tag]
         :args [?elt]
         :where [[?elt :html/children ?ch]
                 [?ch :html/element ?tag]]})
  (entity (:db/id html)))

(q @local-replica
  (plan {:select [?text]
         :args [?root]
         :where #{[[?root :html/children ?ch]
                   [[?text] (recur ?ch)]]
                  [[?root :html/text ?text]]
                  }})
  (entity (:db/id html)))

(swap! ra s/write [{:db/id (:db/id html)
                    :html/children {:html/text "z"
                                    :db/id "whatever"}}])

(clojure.pprint/pprint
  (q @local-replica
    (plan {:select [?text ?e ?write]
           :where [[?e :html/text ?text ?write]]})))

(clojure.pprint/pprint
  (q @local-replica
    (plan {:select [?text ?e]
           :where [[?e :html/text ?text]]})))

(defn entity->graph
  "Returns [root graph]"
  [entity]
  [(:db/id entity) (reduce
                     (fn [g t] (assoc g (:e t) t))
                     {}
                     (s/as-tuples entity))])

#_ ; TODO
#+clj
(defn ->strings
  "Returns a lazy seq of strings for [graph] starting at [root]"
  [root graph])

#+cljs
(defn- create-element
  [tuples]
  (-> (filter (comp #{:html/element} :a) tuples)
    first
    :v
    (or "span")
    name
    dom/createElement
    (doto (dom/setProperties (clj->js {"id" (-> tuples first :e deref)})))))

#+cljs
(defn- update-element
  [{:keys [elt tuples]}]
  (doseq [{:keys [a v]} tuples]
    (case a
      :html/attrs (doseq [[a v] (if (coll? v) v [v])]
                    (dom/setProperties elt (clj->js {(name a) v})))
      :html/text (dom/setTextContent elt v)
      nil))
  {:elt elt :tuples tuples})

(defn- q-sibling-ranks
  [space child-e]
  (q space
    (plan {:select [?parent ?rank ?sibling]
           :args [?child]
           :where [[?parent :html/children ?child]
                   [?parent :html/children ?sibling]
                   [?sibling :html/rank ?rank]]})
    child-e))

#+cljs
(defn- place-elements
  [space elements+tuples]
  ;; TODO hyper-temporary
  (dom/setProperties (first (dom/$$ "body")) (clj->js {"contentEditable" "true"}))
  ;(println elements+tuples)
  (let [q-sibling-ranks (memoize (partial q-sibling-ranks space))
        place (fn place [placed? [eid {:keys [elt tuples]}]]
                (if (placed? eid)
                  placed?
                  (let [siblings (seq (q-sibling-ranks eid))
                        parent-eid (ffirst siblings)
                        placed? (or (and (not parent-eid) placed?)
                                    (and (dom/$ @parent-eid) placed?)
                                    (or
                                      ;(println parent-eid (find elements+tuples @parent-eid))
                                      (place placed? (find elements+tuples @parent-eid))))
                        ]
                    ; TODO need to verify that existing parent is the same as new? parent
                    ; (some-> elt dom/getParentElement .-id (= parent-eid))
                        
                    (dom/removeNode elt)
                    ;(println "adding" elt "to" parent-eid)
                    (-> (or (some-> parent-eid deref dom/$)
                          (-> "body" dom/$$ first))
                      (dom/appendChild elt))
                    (conj placed? eid))))]
    (reduce place #{} elements+tuples)))

#+cljs
(defn tuples->DOM
  "Deposits [tuples] (presumed to all be from one write) into the given browser
[dom] using [space] to lookup relative ranks, etc."
  [tuples space]
  (->> tuples
    (filter #(= "html" (namespace (:a %))))
    (sort (index-types [:e :a :v :write :remove]))
    (partition-by :e)
    (map (fn [tuples] {:tuples tuples
                       :elt (or (dom/getElement @(:e (first tuples)))
                              (create-element tuples))}))
    (map update-element)
    (map (fn [x] [(-> x :tuples first :e) x]))
    (into {})
    (place-elements space)))

#+cljs
(defn ->DOM
  "Deposits [graph] starting at [root] into the given browser [element]"
  [element root graph]
  )

#+cljs
(defn element->graph
  "Returns a graph rooted at the given browser [element]"
  [element])

#+cljs
(defn join
  [write]
  (tuples->DOM write (swap! local-replica s/write write)))

