(ns p79.crdt.space.replication
  (:require [p79.crdt.space :as s :refer (write q)]
            [p79.crdt.space.memory :as mem :refer (in-memory)]
    [clojure.pprint :as pp])
  (:use clojure.test))

(deftest in-memory-atoms
  (let [src (atom (in-memory))
        tgt (atom (in-memory))]
    (s/watch-changes src (partial s/replicate-last-write tgt))
    (swap! src s/write [{:a 5 :db/id "foo"}])
    (let [query '{:select [?k ?v ?write ?write-time]
                       :where [["foo" ?k ?v ?write]
                               [?write ::s/write-time ?write-time]]}
          [sr] (seq (q @src query))
          [tr] (seq (q @tgt query))]
      (is (= (butlast sr) (butlast tr)))
      (is (pos? (compare (last tr) (last sr)))))))

(deftest in-memory-agent
  (let [src (agent (in-memory))
        tgt (agent (in-memory))]
    (s/watch-changes src (partial s/replicate-last-write tgt))
    (send src s/write [{:a 5 :db/id "foo"}])
    (await src tgt)
    (let [query '{:select [?k ?v ?write ?write-time]
                       :where [["foo" ?k ?v ?write]
                               [?write ::s/write-time ?write-time]]}
          [sr] (seq (q @src query))
          [tr] (seq (q @tgt query))]
      (println (q @tgt query))
      (is (= (butlast sr) (butlast tr)))
      (is (pos? (compare (last tr) (last sr)))))))

