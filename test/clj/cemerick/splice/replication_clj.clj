(ns cemerick.splice.replication-clj
  (:require [cemerick.splice :as s :refer (write)]
            [cemerick.splice.memory.query :refer (q)]
            [cemerick.splice.replication :as rep]
            [cemerick.splice.memory :as mem :refer (in-memory)]
            [clojure.java.io :as io]
            [cemerick.splice.uuid :refer (random-uuid)])
  (:use clojure.test))

(defn- read-seq
  [source]
  (letfn [(value-seq [stream]
            (lazy-seq
              (binding [*read-eval* false]
                (if-let [x (read stream false nil)]
                  (cons x (value-seq stream))
                  (.close stream)))))]
    (value-seq (java.io.PushbackReader. (io/reader source)))))

(deftest in-memory-agent
  (let [src (agent (in-memory))
        tgt (agent (in-memory))]
    (rep/watch-changes src (comp (partial rep/write*-to-reference tgt) rep/write-change))
    (send src s/write [{:a 5 :db/eid "foo"}])
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
    (rep/watch-changes src (comp (partial rep/tuples->disk (.getAbsolutePath f)) rep/write-change))
    (dotimes [x 1]
      (swap! src s/write [{:a x :db/eid (random-uuid)}]))
    (let [replica (->> (read-seq (.getAbsolutePath f))
                    (map (partial apply s/coerce-tuple))
                    in-memory)]
      (is (= (.indexes @src) (.indexes replica))))))
