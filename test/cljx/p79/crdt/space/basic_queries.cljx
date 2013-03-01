(ns p79.crdt.space.basic-queries
  (:require [p79.crdt.space :as s :refer (write q)]
            [p79.crdt.space.memory :refer (in-memory)]
            ^:clj [p79.crdt.space.memory.planning :refer (plan)]
            [port79.uuid :refer (time-uuid)]
            ^:clj [clojure.test :as t :refer :all]
            ^:cljs [cemerick.cljs.test :as t])
  ^:cljs (:require-macros [p79.crdt.space.memory.planning :refer (plan)]
                          [cemerick.cljs.test :refer (deftest is are)]))

(deftest basic-queries
  (let [s1 (write (in-memory) [{:a 6 :b 12 :db/id "x"}])
        s2 (write s1 {:some-meta :p} [{:b 6 :db/id "y"}])
        space (write s2 {:some-meta true} [{:b "c" :db/id "y"}])]
    (are [result query] (= (set result) (set (q space (plan query))))
      [[#entity "x" 6]] {:select [?e ?v]
                         :where [[?e :b 12]
                                 [?e :a ?v]]}
      ; entity-position coercion
      [[:a] [:b]] {:select [?a]
                   :where [["x" ?a]]}
      
      [[:a]] {:select [?a]
              :where [["x" ?a 6]]}
      
      ; result narrowing
      [[#entity "x"] [#entity "y"]] {:select [?e]
                                     :where [[?e :b]]}
      [[#entity "x"]] {:select [?e]
                       :where [[?e :b]
                               [?e :a 6]]}
      [[:a] [:b]] {:select [?a]
                   :where [[_ ?a 6]]}
      [[:b]] {:select [?a]
              :where [[_ ?a 6]
                      [_ ?a 12]]}
     
      ; "user"-provided meta
      [[true] [:p]] {:select [?some-meta]
                     :where [["y" _ _ ?t]
                             [?t :some-meta ?some-meta]]}
      [[#entity "y" true] [#entity "y" :p]] {:select [?e ?some-meta]
                                             :where [[?t :some-meta ?some-meta]
                                                     [?e :b _ ?t]]}
      
      ; unbound select
      [] {:select [?e ?v] :where [[?e :b 6]]}
      
      ; disjunction
      [[#entity "x"] [#entity "y"]] {:select [?e]
                                     :where #{[[?e :b 12]]
                                              [[?e :b 6]]}}
      [[#entity "y" :p] [#entity "x" 6]] {:select [?e ?v]
                                          :where [[?e :b]
                                                  #{[[?e :a ?v]]
                                                    [[?t :some-meta ?v]
                                                     ; TODO this pattern should
                                                     ; not require the not=
                                                     ; to avoid matching the ?e
                                                     ; of meta tuples
                                                     [?e _ _ ?t]
                                                     (not= ?e ?t)
                                                     (keyword? ?v)]}]}
      
      ; predicate expressions
      [[#entity "y" "c"]] {:select [?e ?v]
                           :where [(string? ?v)
                                   [?e :b ?v]]}
      [[#entity "y" 6 "c"]] {:select [?e ?v ?s]
                             :where [(number? ?v)
                                     (== (inc ?v) 7)
                                     [?e :b ?v]
                                     [?e :b ?s]
                                     (string? ?s)]}
      
      ; function expressions
      [[6 [0 1]] [12 [0 1 2 3]]] {:select [?v ?range]
                                  :where [[_ :b ?v]
                                          (number? ?v)
                                          [?range (range (/ ?v 3))]]}
      [[6 3] [12 5]] {:select [?v ?k]
                      :where [[_ :b ?v]
                              (number? ?v)
                              [?k (inc (/ ?v 3))]]}
      ; ...with result destructuring
      [[6 0] [12 2]] {:select [?v ?k]
                      :where [[_ :b ?v]
                              (number? ?v)
                              [[_ _ ?k] (range (/ ?v 3) -1 -1)]]}
      
      ; whole-tuple selection
      ;; can't use alias-namespaced keywords b/c of https://github.com/jonase/kibit/issues/14
      [[(s/coerce-tuple "y" :b "c" (-> space meta :p79.crdt.space/last-write) nil)]
       [(s/coerce-tuple "y" :b 6 (-> s2 meta :p79.crdt.space/last-write) nil)]]
      {:select [?t]
       :where [["y" _ _ _ :as ?t]]}
      [[6 (s/coerce-tuple "y" :b 6 (-> s2 meta :p79.crdt.space/last-write) nil)]]
      {:select [?v ?t]
       :where [["y" _ ?v _ :as ?t]
               (-> ?t :v number?)]}
      
      ;; TODO this *should* work, but is bugged
      #_#_
      [[#entity "x" 6 (s/coerce-tuple "y" :b 6 (-> s2 meta :p79.crdt.space/writes first) nil)]]
      {:select [?e ?v ?t]
       :where [["y" _ ?v _ :as ?t]
               (-> ?t :v number?)
               [?e :b ?v]]})
    
    ; entity-reference lookup/coercion / timestamp checks
    (is (neg? (apply compare (first (q space (plan {:select [?xtime ?ytime]
                                                    :where [["x" _ _ ?xwrite]
                                                            [?xwrite :time ?xtime]
                                                            ["y" :b "c" ?ywrite]
                                                            [?ywrite :time ?ytime]]}))))))
    
    ; args
    (is (= #{[#entity "x" :b 12]} (q space (plan {:select [?e $a $v]
                                                  :args [$a $v]
                                                  :where [[?e $a $v]]})
                                    :b 12)))
    ; fn args!
    (is (= #{["c"]} (q space (plan {:select [?v]
                                    :args [?pred]
                                    :where [(?pred ?v)
                                            [_ :b ?v]]})
                      string?)))))