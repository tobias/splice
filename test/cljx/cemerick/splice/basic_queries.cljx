(ns cemerick.splice.basic-queries
  (:require [cemerick.splice :as s :refer (write)]
            [cemerick.splice.memory.query :as q :refer (q)
             ]
            [cemerick.splice.types :refer (entity)]
            [cemerick.splice.memory :as mem :refer (in-memory)]
            [cemerick.splice.rank :as rank]
            #+clj [cemerick.splice.memory.planning :refer (plan)]
            [cemerick.splice.uuid :refer (time-uuid)]
            #+clj [clojure.test :refer :all]
            #+cljs [cemerick.cljs.test :as t])
  #+cljs (:require-macros [cemerick.splice.memory.planning :refer (plan)]
                          [cemerick.cljs.test :refer (deftest is are)]))

(deftest basic-queries
  (let [s1 (write (in-memory) [{:a 6 :b 12 :db/eid "x"}])
        s2 (write s1 {:some-meta :p} [{:b 6 :db/eid "y"}])
        space (write s2 {:some-meta true} [{:b "c" :db/eid "y"}])]
    (are [result query] (= (set result) (set (q space (plan query))))
         [[(entity "x") 6]] {:select [?e ?v]
                             :where [[?e :b 12]
                                     [?e :a ?v]]}
         ; entity-position coercion
         [[:a] [:b]] {:select [?a]
                      :where [["x" ?a]]} 
         
         [[:a]] {:select [?a]
                 :where [["x" ?a 6]]}
         
         ; result narrowing
         [[(entity "x")] [(entity "y")]] {:select [?e]
                                          :where [[?e :b]]}
         [[(entity "x")]] {:select [?e]
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
         [[(entity "y") true] [(entity "y") :p]] {:select [?e ?some-meta]
                                                  :where [[?t :some-meta ?some-meta]
                                                          [?e :b _ ?t]]}
         
         ; unbound select
         [] {:select [?e ?v] :where [[?e :b 6]]}
         
         ; disjunction
         [[(entity "x")] [(entity "y")]] {:select [?e]
                                          :where #{[[?e :b 12]]
                                                   [[?e :b 6]]}}
         [[(entity "y") :p] [(entity "x") 6]] {:select [?e ?v]
                                               :where [[?e :b]
                                                       ; TODO need to propagate
                                                       ; knowledge of ?e bidning into
                                                       ; planning of disjunction so
                                                       ; that [?e _ _ ?t] can have an
                                                       ; index assigned to it
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
         [[(entity "y") "c"]] {:select [?e ?v]
                               :where [(string? ?v)
                                       [?e :b ?v]]}
         [[(entity "y") 6 "c"]] {:select [?e ?v ?s]
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
         [[(s/tuple "y" :b "c" (-> space meta ::mem/last-write) nil)]
          [(s/tuple "y" :b 6 (-> s2 meta ::mem/last-write) nil)]]
         {:select [?t]
          :where [["y" _ _ _ :as ?t]]}
         [[6 (s/tuple "y" :b 6 (-> s2 meta ::mem/last-write) nil)]]
         {:select [?v ?t]
          :where [["y" _ ?v _ :as ?t]
                  (-> ?t :v number?)]}
         
         ;; TODO this *should* work, but is bugged
         #_#_
         [[(entity "x") 6 (s/tuple "y" :b 6 (-> s2 meta ::s/writes first) nil)]]
         {:select [?e ?v ?t]
          :where [["y" _ ?v _ :as ?t]
                  (-> ?t :v number?)
                  [?e :b ?v]]})
    
    ; entity-reference lookup/coercion / timestamp checks
    (is (not (pos? (apply compare (first (q space (plan {:select [?xtime ?ytime]
                                                         :where [["x" _ _ ?xwrite]
                                                                 [?xwrite :db/otime ?xtime]
                                                                 ["y" :b "c" ?ywrite]
                                                                 [?ywrite :db/otime ?ytime]]})))))))
    
    ; args
    (is (= #{[(entity "x") :b 12]} (q space (plan {:select [?e ?a ?v]
                                                   :args [?a ?v]
                                                   :where [[?e ?a ?v]]})
                                     :b 12)))

    ; fn args!
    (is (= #{["c"]} (q space (plan {:select [?v]
                                    :args [?pred]
                                    :where [(?pred ?v)
                                            [_ :b ?v]]})
                      string?)))))

; TODO how is this working *AT ALL*?  
(deftest composite-values
  (let [space (write (in-memory) [{:a [4 5 6] :b #{1 2 3} :db/eid "x"}])]

    (is (thrown-with-msg? #+clj Throwable #+cljs js/Error
          #".*No way to encode value of type.+"
          (write space [{:c #{#{1 2 3}} :db/eid "y"}])))
    
    (are [result query] (= (set result) (set (q space (plan query))))
         [[1] [2] [3]] {:select [?v]
                        :where [[_ :b ?v]]}
         
         [[:b]] {:select [?a]
                 :where [[_ ?a 3]]}
         
         ;; TODO vector values aren't working yet, open question of whether we
         ;; actually want them to or not, anyway
         #_#_
         [[:a]] '{:select [?a]
                  :where [[_ ?a [1 2 3]]]})))

(deftest implicit-disjunctions
  (let [space (-> (in-memory)
                (write [{:b #{1 2 3} :db/eid "x"}])
                (write [{:a #{4 5} :d #{7 8 9} :db/eid "y"}]))]
    (are [result query] (= (set result) (set (q space (plan query))))
      [[:b] [:d]] {:select [?a]
                   :where [[_ ?a #{3 9}]]}
      
      [[1] [2] [3]] {:select [?v]
                     :where [["x" #{:a :b :t} ?v]]}
      
      [[(entity "y")]] {:select [?e]
                       :where [[?e #{:a :d} #{1 4}]]})))

(deftest subqueries
  (let [space (-> (in-memory)
                (write [{:head true :ref (entity "y") :db/eid "x"}
                        {:x 1 :ref (entity "z") :db/eid "y"}
                        {:x 2 :ref (entity "a") :db/eid "z"}
                        {:x 3 :db/eid "a"}]))]
    (is (= #{[(entity "x") (entity "y")] [(entity "x") (entity "z")] [(entity "x") (entity "a")]}
          (q space (plan {:select [?head ?c]
                          :args [?head]
                          :subs {:walk {:select [?c]
                                        :args [?p]
                                        :where #{[[?p :ref ?c]]
                                                 [[?p :ref ?ch]
                                                  [[?c] (q :walk ?ch)]]}}}
                          :where [[[?c] (q :walk ?head)]]})
            (entity "x"))))
    
    (is (= #{[(entity "a") 3] [(entity "y") 1] [(entity "z") 2]}
          (q space
            (plan {:select [?c ?x]
                   :args [?head]
                   :subs {:walk {:select [?c ?x]
                                 :args [?p]
                                 :where #{[[?p :ref ?c]
                                           [?c :x ?x]]
                                          [[?p :ref ?ch]
                                           [[?c ?x] (q :walk ?ch)]]}}}
                   :where [[[?c ?x] (q :walk ?head)]]})
            (entity "x"))
          
          ;; direct-recur
          (q space
            (plan {:select [?c ?x]
                   :args [?p]
                   :where #{[[?p :ref ?c]
                             [?c :x ?x]]
                            [[?p :ref ?ch]
                             [[?c ?x] (recur ?ch)]]}})
            (entity "x"))))))

; <p><em class="title" id="name">x</em><span>y</span></p>
; TODO yeesh, need some convenience for using particular types (e.g. ranks) as
; values of particular entries
(def html (s/assign-map-ids
            {:html/element :p
             :html/rank rank/origin*
             :html/children #{{:html/element :em
                               :html/rank (str (rank/before rank/origin))
                               :html/attrs #{[:class "title"]
                                             [:id "name"]}
                               :html/children {:html/text "x"}}
                              {:html/element :span
                               :html/attrs [:class "foo"]
                               :html/rank (str (rank/after rank/origin))
                               :html/children {:html/text "y"}}}}))

(deftest html-subqueries
  (let [space (write (in-memory) [html])]
    (is (= #{[:em] [:span] [:p]}
          (q space
            (plan {:select [?el]
                   :args [?head]
                   :subs {:walk {:select [?el]
                                 :args [?head]
                                 :where #{[[?head :html/element ?el]]
                                          [[?head :html/children ?ch]
                                           [[?el] (q :walk ?ch)]]}}}
                   :where [[?e :html/element ?head]
                           [[?el] (q :walk ?e)]]})
            :p)
          
          ;; direct-recur
          (q space
            (plan {:select [?el]
                   :args [?head]
                   :where #{[[?head :html/element ?el]]
                            [[?head :html/children ?ch]
                             [[?el] (recur ?ch)]]}})
            (entity (:db/eid html)))))))

(deftest html-attributes
  (let [space (write (in-memory) [html])]
    (are [results query] (= results (q space (plan query)))
      #{[:span [:class "foo"]] [:em [:class "title"]]}
      {:select [?el ?attr]
       :args [?head]
       :where #{[[?head :html/element ?el]
                 [?head :html/attrs ?attr]
                 (= :class (first ?attr))]
                [[?head :html/children ?ch]
                 [[?el ?attr] (recur ?ch)]]}}
      
      #{[:span [:class "foo"]] [:em [:id "name"]] [:em [:class "title"]]}
      {:select [?el ?attr]
       :args [?head]
       :where #{[[?head :html/element ?el]
                 [?head :html/attrs ?attr]]
                [[?head :html/children ?ch]
                 [[?el ?attr] (recur ?ch)]]}})))

(deftest html-subqueries
  (let [m (s/assign-map-ids html)
        space (write (in-memory) [m])]
    (is (= #{[:em] [:span] [:p]}
          (q space
            (plan {:select [?el]
                   :args [?head]
                   :subs {:walk {:select [?el]
                                 :args [?head]
                                 :where #{[[?head :html/element ?el]]
                                          [[?head :html/children ?ch]
                                           [[?el] (q :walk ?ch)]]}}}
                   :where [[?e :html/element ?head]
                           [[?el] (q :walk ?e)]]})
            :p)
          
          ;; direct-recur
          (q space
            (plan {:select [?el]
                   :args [?head]
                   :where #{[[?head :html/element ?el]]
                            [[?head :html/children ?ch]
                             [[?el] (recur ?ch)]]}})
            (entity (:db/eid m)))))))

(deftest deletes
  (let [s (write (in-memory) [{:a #{"y" :x 1 2} :b #{2 4} :db/eid "x"}])]
    (is (= #{[1] [2]} (q s (plan {:select [?v] :where [[_ :a ?v] (number? ?v)]}))))
    (let [attr-writes (q s (plan {:select [?a ?t] :where [[_ ?a 2 ?t]]}))
          remove-tuples (map
                          (fn [[attr write]] (s/tuple "x" attr 2 nil write))
                          attr-writes)
          space (s/write s remove-tuples)]
      
      ;; TODO it would be great to be able to write this query and have it work
      ;; need to eventually figure out how to (automatically?) selectively disable
      ;; the elimination of tuple matches based on removal tuples
      #_
      (pp/pprint [remove-write
                  (-> attr-writes first second)
                  (q s `{:select [?e ?a ?v ?r] :where [[?e ?a ?v ~remove-write ?r]]})])
      #_#_
      (def i (s/index s [:e :a :v :write :remove-write]))
      (def s s)
      ;(pp/pprint i)
      (are [result query] (= (set result) (set (q space (plan query))))
        #{[1]} {:select [?v] :where [[_ :a ?v] (number? ?v)]}
        #{[4]} {:select [?v] :where [[_ :b ?v] (number? ?v)]}
        #{} {:select [?a] :where [[_ ?a 2]]}))))

