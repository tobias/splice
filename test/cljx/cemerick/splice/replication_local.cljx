(ns cemerick.splice.replication-local
  (:require [cemerick.splice :as s :refer (write)]
            [cemerick.splice.memory.query :refer (q)]
            [cemerick.splice.replication :as rep]
            [cemerick.splice.memory :as mem :refer (in-memory)]
            #+clj [cemerick.splice.memory.planning :refer (plan)]
            #+cljs [cemerick.cljs.test :as t])
  #+cljs (:require-macros [cemerick.splice.memory.planning :refer (plan)]
                          [cemerick.cljs.test :refer (deftest is are)])
  #+clj (:use clojure.test))

(deftest in-memory-atoms
  (let [src (atom (in-memory))
        tgt (atom (in-memory))]
    (rep/watch-changes src (comp (partial rep/write*-to-reference tgt)
                                 rep/write-change
                                 ; need some busywork so that the write time of
                                 ; the replicated write is later
                                 #(do (reduce + (range 1e5)) %)))
    (swap! src s/write [{:a 5 :db/eid "foo"}])
    (let [query (plan {:select [?k ?v ?write ?write-time]
                       :where [["foo" ?k ?v ?write]
                               [?write :cemerick.splice/write-time ?write-time]]})
          [sr] (seq (q @src query))
          [tr] (seq (q @tgt query))]
      (is (= (butlast sr) (butlast tr)))
      (is (pos? (compare (last tr) (last sr)))))))

