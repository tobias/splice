(ns p79.crdt.space.replication-local
  (:require [p79.crdt.space :as s :refer (write q)]
            [p79.crdt.space.replication :as rep]
            [p79.crdt.space.memory :as mem :refer (in-memory)]
            ^:clj [p79.crdt.space.memory.planning :refer (plan)]
            ^:cljs [cemerick.cljs.test :as t])
  ^:cljs (:require-macros [p79.crdt.space.memory.planning :refer (plan)]
                          [cemerick.cljs.test :refer (deftest is are)])
  ^:clj (:use clojure.test))

(deftest in-memory-atoms
  (let [src (atom (in-memory))
        tgt (atom (in-memory))]
    (rep/watch-changes src (comp (partial rep/write*-to-reference tgt)
                                 rep/write-change
                                 ; need some busywork so that the write time of
                                 ; the replicated write is later
                                 #(do (reduce + (range 1e5)) %)))
    (swap! src s/write [{:a 5 :db/id "foo"}])
    (let [query (plan {:select [?k ?v ?write ?write-time]
                       :where [["foo" ?k ?v ?write]
                               [?write :p79.crdt.space/write-time ?write-time]]})
          [sr] (seq (q @src query))
          [tr] (seq (q @tgt query))]
      (is (= (butlast sr) (butlast tr)))
      (is (pos? (compare (last tr) (last sr)))))))

