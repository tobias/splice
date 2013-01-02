(ns p79.crdt.space.sanity
  (:require [p79.crdt.space :as s]
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
        fn (#'s/compile-expression-clause '[$a $b] expr)]
    (is (= {:code '(fn [{:syms [$a $b]} {:syms [?y ?x]}] (> 30 (inc ?x) ?y))
            :clause expr}
          (meta fn)))))

(deftest sanity
  (let [space (-> (s/in-memory)
                (s/write [{:a 6 :b 12 :db/id "x"}])
                (s/write [{:b 6 :db/id "y"}])
                (s/write {:some-meta true} [{:b "c" :db/id "y"}]))]
    (are [result query] (= (set result)
                          (set (s/query space query)))
      [[#entity "x" 6]] '{:select [?e ?v]
                          :where [[?e :b 12]
                                  [?e :a ?v]]}
      
      ; entity-position coercion
      [[:a] [:b]] '{:select [?a]
                    :where [["x" ?a]]}
     
      ; "user"-provided meta
      [[true]] '{:select [?some-meta]
                 :where [["y" _ _ ?t]
                         [?t :some-meta ?some-meta]]}
      
      ; unbound select
      [] '{:select [?e ?v] :where [[?e :b 6]]}
      
      ; predicate expressions
      [[#entity "y" "c"]] '{:select [?e ?v]
                            :where [(string? ?v)
                                    [?e :b ?v]]}
      [[#entity "y" 6 "c"]] '{:select [?e ?v ?s]
                              :where [(number? ?v)
                                      (== (inc ?v) 7)
                                      [?e :b ?v]
                                      [?e :b ?s]
                                      (string? ?s)]})
    
    ; entity-reference lookup/coercion / timestamp checks
    (is (neg? (apply compare (first (s/query space '{:select [?xtime ?ytime]
                                                     :where [["x" _ _ ?xwrite]
                                                             [?xwrite :time ?xtime]
                                                             ["y" :b "c" ?ywrite]
                                                             [?ywrite :time ?ytime]]})))))
    
    ; args
    (is (= [[#entity "x"]] (s/query space '{:select [?e]
                                            :args [$a $v]
                                            :where [[?e $a $v]]}
                             :b 12)))
    ; fn args!
    (is (= [["c"]] (s/query space '{:select [?e]
                                    :args [$pred]
                                    :where [($pred ?v)
                                            [_ :b ?v]]}
                     string?)))))

#_#_#_
(def p (#'s/plan yy '{:select [?v ?v2]
                      :where [[?e :b 6]
                              [?e :b 7]
                              [?e :c ?v]
                              [?f ?v2 2]
                              [?f :d ?v2]
                              [?f :a _]
                              ]}))

(pp/pprint p)

(pp/pprint (s/query yy '{:select [?a ?v]
                         :where [[_ :b ?v]
                                 [_ ?a ?v]]}))