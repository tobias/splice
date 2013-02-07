(ns p79.crdt.space.replication
  (:require [p79.crdt.space :as s :refer (write q)]
            [p79.crdt.space.memory :as mem :refer (in-memory)]
            [p79.read :refer (read-seq)]
    [clojure.pprint :as pp])
  (:use clojure.test))

(deftest in-memory-atoms
  (let [src (atom (in-memory))
        tgt (atom (in-memory))]
    (s/watch-changes src (comp (partial s/write*-to-reference tgt) s/write-change))
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
    (s/watch-changes src (comp (partial s/write*-to-reference tgt) s/write-change))
    (send src s/write [{:a 5 :db/id "foo"}])
    (await src tgt)
    (let [query '{:select [?k ?v ?write ?write-time]
                       :where [["foo" ?k ?v ?write]
                               [?write ::s/write-time ?write-time]]}
          [sr] (seq (q @src query))
          [tr] (seq (q @tgt query))]
      (is (= (butlast sr) (butlast tr)))
      (is (pos? (compare (last tr) (last sr)))))))

(deftest to-disk-and-back
  (let [src (atom (in-memory))
        f (java.io.File/createTempFile "replication" ".tuples")]
    (s/watch-changes src (comp (partial s/tuples->disk (.getAbsolutePath f)) s/write-change))
    (dotimes [x 1]
      (swap! src s/write [{:a x :db/id (s/uuid)}]))
    (let [replica (->> (read-seq (.getAbsolutePath f))
                    (map (partial apply s/coerce-tuple))
                    in-memory)]
      (is (= (.indexes @src) (.indexes replica))))))