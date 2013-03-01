(ns p79.crdt.space.sanity
  (:require [p79.crdt.space :as s :refer (write q)]
            [p79.crdt.space.types :refer (entity)]
            [p79.crdt.space.memory :refer (in-memory)]
            [p79.crdt.space.memory.planning :as planning]
            [port79.uuid :refer (time-uuid)]
            [clojure.pprint :as pp])
  (:use clojure.test))

(deftest composite-values
  (let [space (-> (in-memory)
                (write [{:a [4 5 6] :b #{1 2 3} :db/id "x"}])
                (write [{:c #{#{1 2 3}} :d #{7 8 9} :db/id "y"}]))]
    (are [result query] (= (set result) (set (q space query)))
      [[1] [2] [3]] '{:select [?v]
                      :where [[_ :b ?v]]}
      
      [[#{1 2 3}]] '{:select [?v]
                     :where [[_ :c ?v]]}
      
      [[:b]] '{:select [?a]
               :where [[_ ?a 3]]}
      
      ;; TODO vector and set values aren't working yet
      
      #_#_
      [[:a]] '{:select [?a]
               :where [[_ ?a [1 2 3]]]}
      #_#_
      [[:c]] '{:select [?a]
               :where [[_ ?a #{#{1 2 3}}]]}
      )))

(deftest implicit-disjunctions
  (let [space (-> (in-memory)
                (write [{:b #{1 2 3} :db/id "x"}])
                (write [{:a #{4 5} :d #{7 8 9} :db/id "y"}]))]
    (are [result query] (= (set result) (set (q space query)))
      [[:b] [:d]] '{:select [?a]
                    :where [[_ ?a #{3 9}]]}
      
      [[1] [2] [3]] '{:select [?v]
                      :where [["x" #{:a :b :t} ?v]]}
      
      [[#entity "y"]] '{:select [?e]
                        :where [[?e #{:a :d} #{1 4}]]})))

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
            (entity (:db/id m)))))))

(deftest deletes
  (let [s (write (in-memory) [{:a #{"y" :x 1 2} :b #{2 4} :db/id "x"}])]
    (is (= #{[1] [2]} (q s '{:select [?v] :where [[_ :a ?v] (number? ?v)]})))
    (let [attr-writes (q s '{:select [?a ?t] :where [[_ ?a 2 ?t]]})
          remove-write (time-uuid)
          remove-tuples (map
                          (fn [[attr write]] (s/coerce-tuple "x" attr 2 remove-write write))
                          attr-writes)
          s (s/write* s remove-write remove-tuples)]
      
      ;; TODO it would be great to be able to write this query and have it work
      ;; need to eventually figure out how to (automatically?) selectively disable
      ;; the elimination of tuple matches based on removal tuples
      #_
      (pp/pprint [remove-write
                  (-> attr-writes first second)
                  (q s `{:select [?e ?a ?v ?r] :where [[?e ?a ?v ~remove-write ?r]]})])
      #_#_
      (def i (s/index s [:e :a :v :write :remove]))
      (def s s)
      ;(pp/pprint i)
      (is (= #{[1]} (q s '{:select [?v] :where [[_ :a ?v] (number? ?v)]})))
      (is (= #{[4]} (q s '{:select [?v] :where [[_ :b ?v] (number? ?v)]})))
      (is (= #{} (q s '{:select [?a] :where [[_ ?a 2]]}))))))

