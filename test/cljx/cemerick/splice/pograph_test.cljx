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
                          [cemerick.cljs.test :refer (deftest is are)]))


(def splice (atom (in-memory)))

(defn- po-attrs
  [base-attr values]
  (for [[idx x] (map-indexed vector values)]
    [(types/po-attr base-attr idx) x]))

(swap! splice s/write [(into {::s/e "m"}
                         (po-attrs :children (map types/reference ["c1" "c2" "c3"])))
                       (into {::s/e "c1"} (po-attrs :children (map str "abc")))
                       (into {::s/e "c2"} (po-attrs :children (map str "xyz")))
                       (into {::s/e "c3"}
                         (po-attrs :children ["j" "k" #ref "i1" "l"]))
                       {::s/e "i1" #po-attr [:children "0"] "data"}])

(defn- po-attrs->string
  [attrs]
  (->> (sort-by first attrs)
    (map second)
    (apply str)))


(deftest dumb
  (is (= "jkl" (po-attrs->string
                 (q @splice (plan {:select [?a ?v]
                                   :where [["c3" (<= #po-attr [:children 0] ?a) ?v]
                                           (string? ?v)]}))))))
