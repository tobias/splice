(ns p79.crdt.space.sanity
  (:require [p79.crdt.space :as s :refer (in-memory write q)]
    [clojure.pprint :as pp])
  (:use clojure.test))

(deftest default-planner
  (is (= '[[?e :g ?v] (pos? ?v) [?e :a ?a] (pos? ?a) [?e :b 5 "tag"]]
        (#'s/reorder-expression-clauses
          '[(pos? ?v)
            [?e :g ?v]
            (pos? ?a)
            [?e :a ?a]
            [?e :b 5 "tag"]]))))

(deftest predicate-expression-compilation
  (let [expr '(> 30 (inc ?x) ?y)
        fn (#'s/compile-expression-clause (#'s/clause-bindings expr) expr)]
    (is (= {:code '(fn [{:syms [?y ?x]}] (> 30 (inc ?x) ?y))
            :clause expr}
          (meta fn)))))

(deftest sanity
  (let [space (-> (in-memory)
                (write [{:a 6 :b 12 :db/id "x"}])
                (write {:some-meta :p} [{:b 6 :db/id "y"}])
                (write {:some-meta true} [{:b "c" :db/id "y"}]))]
    (are [result query] (= (set result)
                          (set (q space query)))
      [[#entity "x" 6]] '{:select [?e ?v]
                          :where [[?e :b 12]
                                  [?e :a ?v]]}
      ; entity-position coercion
      [[:a] [:b]] '{:select [?a]
                    :where [["x" ?a]]}
      
      [[:a]] '{:select [?a]
               :where [["x" ?a 6]]}
      
      ; result narrowing
      [[#entity "x"] [#entity "y"]] '{:select [?e]
                                      :where [[?e :b]]}
      [[#entity "x"]] '{:select [?e]
                        :where [[?e :b]
                                [?e :a 6]]}
      [[:a] [:b]] '{:select [?a]
                    :where [[_ ?a 6]]}
      [[:b]] '{:select [?a]
               :where [[_ ?a 6]
                       [_ ?a 12]]}
     
      ; "user"-provided meta
      [[true] [:p]] '{:select [?some-meta]
                      :where [["y" _ _ ?t]
                              [?t :some-meta ?some-meta]]}
      [[#entity "y" true] [#entity "y" :p]] '{:select [?e ?some-meta]
                                              :where [[?t :some-meta ?some-meta]
                                                      [?e :b _ ?t]]}
      
      ; unbound select
      [] '{:select [?e ?v] :where [[?e :b 6]]}
      
      ; disjunction
      [[#entity "x"] [#entity "y"]] '{:select [?e]
                                      :where #{[[?e :b 12]]
                                               [[?e :b 6]]}}
      [[#entity "y" :p] [#entity "x" 6]] '{:select [?e ?v]
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
      [[#entity "y" "c"]] '{:select [?e ?v]
                            :where [(string? ?v)
                                    [?e :b ?v]]}
      [[#entity "y" 6 "c"]] '{:select [?e ?v ?s]
                              :where [(number? ?v)
                                      (== (inc ?v) 7)
                                      [?e :b ?v]
                                      [?e :b ?s]
                                      (string? ?s)]}
      
      ; function expressions
      [[6 [0 1]] [12 [0 1 2 3]]] '{:select [?v ?range]
                                   :where [[_ :b ?v]
                                           (number? ?v)
                                           [?range (range (/ ?v 3))]]}
      [[6 3] [12 5]] '{:select [?v ?k]
                       :where [[_ :b ?v]
                               (number? ?v)
                               [?k (inc (/ ?v 3))]]}
      ; ...with result destructuring
      [[6 0] [12 2]] '{:select [?v ?k]
                       :where [[_ :b ?v]
                               (number? ?v)
                               [[_ _ ?k] (range (/ ?v 3) -1 -1)]]})
    
    ; entity-reference lookup/coercion / timestamp checks
    (is (neg? (apply compare (first (q space '{:select [?xtime ?ytime]
                                                     :where [["x" _ _ ?xwrite]
                                                             [?xwrite :time ?xtime]
                                                             ["y" :b "c" ?ywrite]
                                                             [?ywrite :time ?ytime]]})))))
    
    ; args
    (is (= #{[#entity "x" :b 12]} (q space '{:select [?e $a $v]
                                                   :args [$a $v]
                                                   :where [[?e $a $v]]}
                                    :b 12)))
    ; fn args!
    (is (= #{["c"]} (q space '{:select [?v]
                                     :args [?pred]
                                     :where [(?pred ?v)
                                             [_ :b ?v]]}
                      string?)))))

(deftest subqueries
  (let [space (-> (in-memory)
                (write [{:head true :ref #entity "y" :db/id "x"}
                        {:x 1 :ref #entity "z" :db/id "y"}
                        {:x 2 :ref #entity "a" :db/id "z"}
                        {:x 3 :db/id "a"}]))]
    (is (= #{[#entity "x" #entity "y"] [#entity "x" #entity "z"] [#entity "x" #entity "a"]}
          (q space '{:select [?head ?c]
                     :args [?head]
                     :subs {:walk {:select [?c]
                                   :args [?p]
                                   :where #{[[?p :ref ?c]]
                                            [[?p :ref ?ch]
                                             [[?c] (q :walk ?ch)]]}}}
                     :where [[[?c] (q :walk ?head)]]}
            #entity "x")))
    
    (is (= #{[#entity "a" 3] [#entity "y" 1] [#entity "z" 2]}
          (q space
            '{:select [?c ?x]
              :args [?head]
              :subs {:walk {:select [?c ?x]
                            :args [?p]
                            :where #{[[?p :ref ?c]
                                      [?c :x ?x]]
                                     [[?p :ref ?ch]
                                      [[?c ?x] (q :walk ?ch)]]}}}
              :where [[[?c ?x] (q :walk ?head)]]}
            #entity "x")
          
          ;; direct-recur
          (q space
            '{:select [?c ?x]
              :args [?p]
              :where #{[[?p :ref ?c]
                        [?c :x ?x]]
                       [[?p :ref ?ch]
                        [[?c ?x] (recur ?ch)]]}}
            #entity "x")))))

; <p><em class="title" id="name">x</em><span>y</span></p>
(def html {:html/element :p
           :html/rank 0.5M
           :html/children #{{:html/element :em
                             :html/rank 0.5M
                             :html/children #{{:html/attr :class :value "title"}
                                              {:html/attr :id :value "name"}
                                              {:html/text "x"}}}
                            {:html/element :span
                             :html/rank 0.75M
                             :html/children #{{:html/text "y"}}}}})

(deftest html-subqueries
  (let [m (s/assign-map-ids html)
        space (write (in-memory) [m])]
    (is (= #{[:em] [:span] [:p]}
          (q space
            '{:select [?el]
              :args [?head]
              :subs {:walk {:select [?el]
                            :args [?head]
                            :where #{[[?head :html/element ?el]]
                                     [[?head :html/children ?ch]
                                      [[?el] (q :walk ?ch)]]}}}
              :where [[?e :html/element ?head]
                      [[?el] (q :walk ?e)]]}
            :p)
          
          ;; direct-recur
          (q space
            '{:select [?el]
              :args [?head]
              :where #{[[?head :html/element ?el]]
                       [[?head :html/children ?ch]
                        [[?el] (recur ?ch)]]}}
            (s/entity (:db/id m)))))))

