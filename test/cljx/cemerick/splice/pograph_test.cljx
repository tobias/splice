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
    [(types/po-attr base-attr idx) x]))

(swap! splice s/write [{::s/e "p" [0] "q" [1] "r" :ref #ref "m"}
                       (into {::s/e "m"}
                         (po-attrs :children (map types/reference ["c1" "c2" "c3"])))
                       (into {::s/e "c1"} (po-attrs :children (map str "abc")))
                       (into {::s/e "c2"} (po-attrs :children (map str "xyz")))
                       (into {::s/e "c3"}
                         (po-attrs :children ["j" "k" #ref "i1" "l"]))
                       {::s/e "i1" #po [:children "0"] "data"}])

(defn- po-attrs->string
  [attrs]
  (->> (sort-by first attrs)
    (map second)
    (apply str)))

(deftest dumb
  (is (= "jkl" (po-attrs->string
                 (q @splice (plan {:select [?a ?v]
                                   :where [["c3" (<= #po [:children 0] ?a) ?v]
                                           (string? ?v)]}))))))

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
  (is (= #{[#po [:children 2] "z"] [#po [:children 1] "y"]
           [#po [:children 0] "x"]}
        (set (q @splice (plan {:select [?a ?ch]
                               :where [["c2" ?a ?ch]]})))))

  (is (= #{["x"] ["y"] ["z"]}
        (set (q @splice (plan {:select [?ch]
                               :where [["c2" #po [:children _] ?ch]]})))))

  (is (= #{[:children "x"] [:children "y"] [:children "z"]}
        (set (q @splice (plan {:select [?base-attr ?ch]
                               :where [["c2" #po [?base-attr _] ?ch]]})))))

  (is (= #{[0 "x"] [1 "y"] [2 "z"]}
        (set (q @splice (plan {:select [?rank ?ch]
                               :where [["c2" #po [:children ?rank] ?ch]]}))))))

