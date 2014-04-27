(ns cemerick.splice.pograph-test
  (:require [cemerick.splice :as s :refer (write)]
            [cemerick.splice.memory.query :as q :refer (q)]
            [cemerick.splice.test :refer (set-check)]
            [cemerick.splice.memory :as mem :refer (in-memory)]
            [cemerick.splice.rank :as rank]
            [cemerick.splice.types :as types]
            #+clj [cemerick.splice.memory.planning :refer (plan)]
            [cemerick.splice.uuid :refer (time-uuid)]
            #+clj [clojure.test :refer :all]
            #+cljs [cemerick.cljs.test :as t])
  #+cljs (:require-macros [cemerick.splice.memory.planning :refer (plan)]
                          [cemerick.cljs.test :refer (deftest is are run-tests)]))


(def splice (atom (in-memory)))

(defn- po-attrs
  [base-attr values]
  (for [[idx x] (map-indexed vector values)]
    [(types/oattr base-attr idx) x]))

(swap! splice s/write [{::s/e "p" [0] "q" [1] "r" :ref #ref "m"}
                       (into {::s/e "m"}
                         (po-attrs :children (map types/reference ["c1" "c2" "c3"])))
                       (into {::s/e "c1"} (po-attrs :children (map str "abc")))
                       (into {::s/e "c2"} (po-attrs :children (map str "xyz")))
                       (into {::s/e "c3"}
                         (po-attrs :children ["j" "k" #ref "i1" "l"]))
                       {::s/e "i1" #oa [:children "0"] "data"}])

(defn- po-attrs->string
  [tuples]
  (->> (sort-by :a tuples)
    (map :v)
    (apply str)))

(deftest dumb
  (is (= "jkl" (po-attrs->string
                 (apply concat
                   (q @splice (plan {:select [?t]
                                     ; TODO good example of where clamping scan to a single partition
                                     ; (just strings here) would be helpful (eliminate the predicate)
                                     :where [["c3" (<= #oa [:children _] ?a) ?v _ :as ?t]
                                             (string? ?v)]})))))))

; TODO should go elsewhere, not related to pographs
(deftest bind-aggregate-types
  (is (= [["q"]] (q @splice (plan {:select [?v]
                                   :args [?i]
                                   :where [["p" [?i] ?v]]})
                   0)))

  (is (= [[0]] (q @splice (plan {:select [?i]
                                 :where [["p" [?i] "q"]]}))))

  (is (= [["m"]] (q @splice (plan {:select [?eid]
                                   :where [["p" :ref #ref ?eid]]}))))

  (is (= [[#ref "m"]] (q @splice (plan {:select [?ref]
                                        :where [["p" :ref ?ref]]})))))

(deftest po-attr-matching
  (is (= #{[#oa [:children 2] "z"] [#oa [:children 1] "y"]
           [#oa [:children 0] "x"]}
        (set (q @splice (plan {:select [?a ?ch]
                               :where [["c2" ?a ?ch]]})))))

  (is (= #{["x"] ["y"] ["z"]}
        (set (q @splice (plan {:select [?ch]
                               :where [["c2" #oa [:children _] ?ch]]})))))

  (is (= #{[:children "x"] [:children "y"] [:children "z"]}
        (set (q @splice (plan {:select [?base-attr ?ch]
                               :where [["c2" #oa [?base-attr _] ?ch]]})))))

  (is (= #{[0 "x"] [1 "y"] [2 "z"]}
        (set (q @splice (plan {:select [?rank ?ch]
                               :where [["c2" #oa [:children ?rank] ?ch]]}))))))

(deftest unbox-references-when-matching
  (is (= (into #{#ref "i1"} (map str "abcxyzjkl"))
        (set (mapcat identity (q @splice (plan {:select [?v]
                                                :where [["m" _ ?ch]
                                                        [?ch _ ?v]]})))))))





(defn- fence-path
  [splice root-eid base-attr [anchor-eid anchor-rank]]
  )

(defn- fence-tuples
  [splice root-eid base-attr [start-eid start-rank :as start] [end-eid end-rank :as end]]
  )

(defn- fence
  [splice base-attr [start-eid start-rank :as start] [end-eid end-rank :as end]]
  (let [siblings (->> (q splice (plan {:select [?a ?v]
                                                   :args [?start-eid ?start-attr]
                                                   :where [[?start-eid (>= ?a ?start-attr) ?v]]})
                                    start-eid (types/oattr base-attr start-rank))
                   (sort-by first)
                   (map second))]
    (mapcat #(concat
               (when (types/reference? %)
                 (fence splice base-attr [@% s/index-bottom] [@% s/index-top]))
               [%])
      siblings))
  )

(deftest manual-fence
  (let [endpoint-query (plan {:select [?e ?a]
						:args [??v]
						:where [[?e ?a ?v]
							(= ??v ?v)]})
        [start end] (map #(q @splice endpoint-query %) ["c" "k"])]
    (def fence (mapcat identity [start end]))))
