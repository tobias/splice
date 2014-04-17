(ns cemerick.splice.scan-range-queries
  (:require [cemerick.splice :as s :refer (write)]
            [cemerick.splice.memory.query :as q :refer (q)]
            [cemerick.splice.test :refer (set-check)]
            [cemerick.splice.memory :as mem :refer (in-memory)]
            [cemerick.splice.rank :as rank]
            #+clj [cemerick.splice.memory.planning :refer (plan) :as plan]
            [cemerick.splice.uuid :refer (time-uuid)]
            #+clj [clojure.test :refer :all]
            #+cljs [cemerick.cljs.test :as t])
  #+cljs (:require-macros [cemerick.splice.memory.planning :refer (plan)]
                          [cemerick.cljs.test :refer (deftest is are)]))

;; TODO want to be able to specify component of scan range expression without binding anything, e.g.
;; (>= 5) should match values >= 5, but currently requires (>= ?x 5)

; TODO how to characterize order? Implicit from the different range expressions
; in match clauses?
;
; #optimization I'd expect the results of this query to be in the order of the
; index used in the scan range clause:
(comment 
  (def a (atom (mem/in-memory)))
  (dotimes [x 100] (swap! a s/write [{::s/e "m" :x x}]))
  (q @a (plan {:select [?w]
               :where [[_ _ _ (< true ?w)]]})))

(def space (-> (in-memory)
             (write [{:a 6 :b 12 ::s/e "x"}])
             (write [{:b 6 ::s/e "y"}])
             (write [{:b "c" ::s/e "y"}])))

(deftest basic
  (are [result query] (= (set result) (set-check (q space (plan query))))
       #{[12] ["c"]} {:select [?v]
                      :where [[_ :b (< 7 ?v)]]}

       ; inclusive, exclusive ranges
       #{[12] ["c"]} {:select [?v]
                      :where [[_ :b (< 6 ?v)]]}

       #{[12] ["c"] [6]} {:select [?v]
                          :where [[_ :b (<= 6 ?v)]]}

       #{[12] [6]} {:select [?v]
                    :where [[_ :b (<= 6 ?v 500)]]}

       #{[12]} {:select [?v]
                :where [[_ :b (< 6 ?v 500)]]}))

(deftest with-args
  (are [result query args] (= (set result)
                             (set-check (apply q space (plan query) args)))

       #{[12] ["c"]}
       {:select [?v]
        :args [?bottom]
        :where [[_ :b (< ?bottom ?v)]]}
       [6]

       #{[12]}
       {:select [?v]
        :args [?bottom ?top]
        :where [[_ :b (< ?bottom ?v ?top)]]}
       [6 "a"]))
