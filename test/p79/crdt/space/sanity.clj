(ns p79.crdt.space.sanity
  (:require [p79.crdt.space :as s]
    [clojure.pprint :as pp])
  (:use clojure.test))

(deftest sanity
  (let [space (-> (s/in-memory)
                (s/write [{:a 6 :b 12 :db/id "x"}])
                (s/write [{:b 6 :db/id "y"}])
                (s/write [{:b "c" :db/id "y"}]))]
    (are [result query] (= (set result)
                          (set(s/query space query)))
      [[#entity "x" 6]] '{:select [?e ?v]
                          :where [[?e :b 12]
                                  [?e :a ?v]]}
      [[:a] [:b]] '{:select [?a]
                    ; TODO make #entity implicit in e position
                    :where [[#entity "x" ?a]]}
      
      #_#_
      [[#entity "y" "c"]
       [#entity "y" 6]] '{:select [?e ?v]
                          :where [[?xe :a _]
                                  [?xe ?a 12]
                                  [?e ?a 6]]})))

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