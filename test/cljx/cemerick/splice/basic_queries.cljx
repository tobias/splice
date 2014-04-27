(ns cemerick.splice.basic-queries
  (:require [cemerick.splice :as s :refer (write)]
            [cemerick.splice.memory.query :as q :refer (q)]
            [cemerick.splice.test :refer (set-check)]
            [cemerick.splice.memory :as mem :refer (in-memory)]
            [cemerick.splice.rank :as rank]
            #+clj [cemerick.splice.memory.planning :refer (plan)]
            [cemerick.splice.uuid :refer (time-uuid)]
            #+clj [clojure.test :refer :all]
            #+cljs [cemerick.cljs.test :as t])
  #+cljs (:require-macros [cemerick.splice.memory.planning :refer (plan)]
                          [cemerick.cljs.test :refer (deftest is are)]))

; want to be able to write something like this:
; (defaulting bindings)
#_
(cemerick.splice.memory.query/q @local
		   (cemerick.splice.memory.planning/plan
		     {:select [?ref ?rank ?text]
		      :args [?head]
		      :where #{[[?head :html/text ?text]
				[?head :rank ?rank]
				]
			       [[?head :html/children ?ref]
				[[?rank ?text] [nil nil]]
				]}})
		   "388e25ed-5d32-4c5b-be9e-b50f7b6524c2")

; broken: ?eid is sometimes unbound, but the `subs` expression is still evaluated
#_
(defn- chars-within-root
  [space eid]
  (cemerick.splice.memory.query/q space
    (cemerick.splice.memory.planning/plan
      {:select [?e ?t]
       :args [?head]
       :subs {:children {:select [?head ?t]
                         :args [?head]
                         :where #{[[?head :html/text ?t]]
                                  [[?head :html/children ?ch]
                                   [[?e ?t] (recur ?ch)]]}}}
       :where [[[?eid ?t] (q :children ?head)]
               [?e (subs ?eid 0 8)]]})
    eid))
;data
#_
[["-local-config" :local.quilt/site-id "d29e7d1d-424f-4993-a7ad-8193dd1b9e89" ["d29e7d1d-424f-4993-a7ad-8193dd1b9e89" 1]] ["00fa3aa8-0b56-4153-9e3b-84865b6b0f03" :rank "Ͽ" ["d29e7d1d-424f-4993-a7ad-8193dd1b9e89" 2]] ["00fa3aa8-0b56-4153-9e3b-84865b6b0f03" :html/text "x" ["d29e7d1d-424f-4993-a7ad-8193dd1b9e89" 2]] ["86a76c5e-e0ac-4d58-bb1e-374c242c28f3" :html/children "98d7dc6c-5f7c-454b-af99-d5e17b1e6e25" ["d29e7d1d-424f-4993-a7ad-8193dd1b9e89" 2]] ["98d7dc6c-5f7c-454b-af99-d5e17b1e6e25" :rank "Ͽ" ["d29e7d1d-424f-4993-a7ad-8193dd1b9e89" 2]] ["98d7dc6c-5f7c-454b-af99-d5e17b1e6e25" :doc/element :block ["d29e7d1d-424f-4993-a7ad-8193dd1b9e89" 2]] ["98d7dc6c-5f7c-454b-af99-d5e17b1e6e25" :html/children "00fa3aa8-0b56-4153-9e3b-84865b6b0f03" ["d29e7d1d-424f-4993-a7ad-8193dd1b9e89" 2]] [["d29e7d1d-424f-4993-a7ad-8193dd1b9e89" 1] :clock/wall #inst "2014-03-10T11:01:31.948-00:00" ["d29e7d1d-424f-4993-a7ad-8193dd1b9e89" 1]] [["d29e7d1d-424f-4993-a7ad-8193dd1b9e89" 2] :clock/wall #inst "2014-03-10T11:01:35.761-00:00" ["d29e7d1d-424f-4993-a7ad-8193dd1b9e89" 2]]]


(deftest basic-queries
  (let [s1 (write (in-memory) [{:a 6 :b 12 :c ["j" "k"] ::s/e "x"}])
        s2 (write s1 {:some-meta :p} [{:b 6 ::s/e "y"}])
        space (write s2 {:some-meta true} [{:b "c" ::s/e "y"}])]
    (are [result query] (= (set result) (set-check (q space (plan query))))
         [[(identity "x") 6]] {:select [?e ?v]
                             :where [[?e :b 12]
                                     [?e :a ?v]]}
         ; entity-position coercion
         [[:a] [:b] [:c]] {:select [?a]
                           :where [["x" ?a]]} 
         
         [[:a]] {:select [?a]
                 :where [["x" ?a 6]]}
         
         ; result narrowing
         [[(identity "x")] [(identity "y")]] {:select [?e]
                                          :where [[?e :b]]}
         [[(identity "x")]] {:select [?e]
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
         [[(identity "y") true] [(identity "y") :p]] {:select [?e ?some-meta]
                                                  :where [[?t :some-meta ?some-meta]
                                                          [?e :b _ ?t]]}
         
         ; unbound select
         ; TODO re-enable this once we can distinguish between bound nil values,
         ; and unbound variables; the former are OK, the latter are _at least_ a
         ; plan-time warning, and maybe a hard failure
         ; [] {:select [?e ?v] :where [[?e :b 6]]}
         
         ; disjunction
         [[(identity "x")] [(identity "y")]] {:select [?e]
                                          :where #{[[?e :b 12]]
                                                   [[?e :b 6]]}}
         [[(identity "y") :p] [(identity "x") 6]] {:select [?e ?v]
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
         [[(identity "y") "c"]] {:select [?e ?v]
                               :where [(string? ?v)
                                       [?e :b ?v]]}
         [[(identity "y") 6 "c"]] {:select [?e ?v ?s]
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
         ; ...destructuring existing binding directly w/o fn expr
         [[["j" "k"] "j" "k"]] {:select [?v ?first ?second]
                                :where [[_ :c ?v]
                                        [[?first ?second] ?v]]}

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
         [[(identity "x") 6 (s/tuple "y" :b 6 (-> s2 meta ::s/writes first) nil)]]
         {:select [?e ?v ?t]
          :where [["y" _ ?v _ :as ?t]
                  (-> ?t :v number?)
                  [?e :b ?v]]})
    
    ; entity-reference lookup/coercion / timestamp checks
    (is (not (pos? (apply compare (first (q space (plan {:select [?xtime ?ytime]
                                                         :where [["x" _ _ ?xwrite]
                                                                 [?xwrite :clock/wall ?xtime]
                                                                 ["y" :b "c" ?ywrite]
                                                                 [?ywrite :clock/wall ?ytime]]})))))))
    
    ; args
    (is (= [[(identity "x") :b 12]] (q space (plan {:select [?e ?a ?v]
                                               :args [?a ?v]
                                               :where [[?e ?a ?v]]})
                                 :b 12)))

    ; fn args!
    (is (= [["c"]] (q space (plan {:select [?v]
                                    :args [?pred]
                                    :where [(?pred ?v)
                                            [_ :b ?v]]})
                      string?)))))

; TODO how is this working *AT ALL*?  
(deftest composite-values
  (let [space (write (in-memory) [{:a [4 5 6] :b #{1 2 3} ::s/e "x"}])]

    (is (thrown-with-msg? #+clj Throwable #+cljs js/Error
          #".*No way to encode value of type.+"
          (write space [{:c #{#{1 2 3}} ::s/e "y"}])))
    
    (are [result query] (= (set result) (set-check (q space (plan query))))
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
                (write [{:b #{1 2 3} ::s/e "x"}])
                (write [{:a #{4 5} :d #{7 8 9} ::s/e "y"}]))]
    (are [result query] (= (set result) (set-check (q space (plan query))))
      [[:b] [:d]] {:select [?a]
                   :where [[_ ?a #{3 9}]]}
      
      [[1] [2] [3]] {:select [?v]
                     :where [["x" #{:a :b :t} ?v]]}
      
      [[(identity "y")]] {:select [?e]
                       :where [[?e #{:a :d} #{1 4}]]})))

(deftest subqueries
  (let [space (-> (in-memory)
                (write [{:head true :ref (identity "y") ::s/e "x"}
                        {:x 1 :ref (identity "z") ::s/e "y"}
                        {:x 2 :ref (identity "a") ::s/e "z"}
                        {:x 3 ::s/e "a"}]))]
    (is (= #{[(identity "x") (identity "y")] [(identity "x") (identity "z")] [(identity "x") (identity "a")]}
          (set-check (q space (plan {:select [?head ?c]
                           :args [?head]
                           :subs {:walk {:select [?c]
                                         :args [?p]
                                         :where #{[[?p :ref ?c]]
                                                  [[?p :ref ?ch]
                                                   [[?c] (q :walk ?ch)]]}}}
                           :where [[[?c] (q :walk ?head)]]})
             (identity "x")))))
    
    (is (= #{[(identity "a") 3] [(identity "y") 1] [(identity "z") 2]}
          (set-check (q space
             (plan {:select [?c ?x]
                    :args [?head]
                    :subs {:walk {:select [?c ?x]
                                  :args [?p]
                                  :where #{[[?p :ref ?c]
                                            [?c :x ?x]]
                                           [[?p :ref ?ch]
                                            [[?c ?x] (q :walk ?ch)]]}}}
                    :where [[[?c ?x] (q :walk ?head)]]})
             (identity "x")))
          
          ;; direct-recur
          (set-check (q space
             (plan {:select [?c ?x]
                    :args [?p]
                    :where #{[[?p :ref ?c]
                              [?c :x ?x]]
                             [[?p :ref ?ch]
                              [[?c ?x] (recur ?ch)]]}})
             (identity "x")))))))

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
          (set-check (q space
             (plan {:select [?el]
                    :args [?head]
                    :subs {:walk {:select [?el]
                                  :args [?head]
                                  :where #{[[?head :html/element ?el]]
                                           [[?head :html/children ?ch]
                                            [[?el] (q :walk ?ch)]]}}}
                    :where [[?e :html/element ?head]
                            [[?el] (q :walk ?e)]]})
             :p))
          
          ;; direct-recur
          (set-check (q space
             (plan {:select [?el]
                    :args [?head]
                    :where #{[[?head :html/element ?el]]
                             [[?head :html/children ?ch]
                              [[?el] (recur ?ch)]]}})
             (identity (::s/e html))))))))

(deftest html-attributes
  (let [space (write (in-memory) [html])]
    (are [results query] (= results (set-check (q space (plan query))))
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

(deftest deletes
  (let [s (-> (in-memory)
            (write [{:a #{"y" :x 1 2} ::s/e "x"}])
            (write [{:b #{2 4} ::s/e "x"}]))]
    (is (= #{[1] [2]} (set-check (q s (plan {:select [?v] :where [[_ :a ?v] (number? ?v)]})))))
    (let [attr-writes (into {} (set-check (q s (plan {:select [?a ?t] :where [[_ ?a 2 ?t]]}))))
          remove-tuples (map
                          (fn [[attr write]] (s/tuple "x" attr 2 nil write))
                          attr-writes)
          space (s/write s remove-tuples)
          remove-write (-> space meta ::mem/last-write)]

      
      
      (are [result query] (= (set result) (set-check (q space (plan query))))
        #{[1]} {:select [?v] :where [[_ :a ?v] (number? ?v)]}
        #{[4]} {:select [?v] :where [[_ :b ?v] (number? ?v)]}
        #{} {:select [?a] :where [[_ ?a 2]]})

      ;; trawl some history
      (doseq [[a w rw] (q space (plan {:select [?a ?w ?wr]
                                             :where [[_ ?a 2 ?w ?wr]]}))]
        (if rw
          (do (is (= w remove-write))
              (is (= rw (attr-writes a))))
          (is (= w (attr-writes a))))))))

