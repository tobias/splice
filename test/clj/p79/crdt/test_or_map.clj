(ns p79.crdt.test-or-map
  (:require [p79.crdt :as crdt]
            [p79.crdt.map :as map]
            [p79.crdt.or-map :refer (create)]
            [p79.vclock :as vc])
  (:use clojure.test)
  (:refer-clojure :exclude (replicate)))

(deftest sanity
  (let [a (create :x 1 :y 2 :z 3 :j 4 :k 5)
        b (map/remove a :x)
        c (map/remove b :y)
        d (map/add c :w 6)
        e (map/add a :v 6)
        f (map/remove b :z)]
    (is (= a {:x 1 :y 2 :z 3 :j 4 :k 5}))
    (is (= b {:y 2 :z 3 :j 4 :k 5}))
    (is (= c {:z 3 :j 4 :k 5}))
    (is (= d {:z 3 :j 4 :k 5 :w 6}))
    (is (= e {:x 1 :y 2 :z 3 :j 4 :k 5 :v 6}))
    (is (= f {:y 2 :j 4 :k 5}))))


