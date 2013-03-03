(ns p79.crdt.test-or-set
  (:require [p79.crdt :as crdt]
            [p79.crdt.set :as cset]
            [p79.crdt.or-set :as or-set]
            [p79.vclock :as vc])
  (:use clojure.test)
  (:refer-clojure :exclude (replicate)))

(deftest sanity
  (let [a (or-set/create 1 2 3 4 5)
        b (cset/remove a 2)
        c (cset/remove b 4)
        d (cset/add c 6)
        e (cset/add a 6)
        f (cset/remove b 4)]
    (is (= a #{1 2 3 4 5}))
    (is (= b #{1 3 4 5}))
    (is (= c #{1 3 5}))
    (is (= d #{1 3 5 6}))
    (is (= e #{1 2 3 4 5 6}))
    (is (= f #{1 3 5}))
    (is (= (p79.crdt/join e d) #{6 5 3 1}))))

(deftest eventual-consistency
  (let [a (or-set/create 1 2 3 4 5)
        b (or-set/create 4 5 6 7 8)
        a1 (p79.crdt/join a
             (or-set/->ObservedRemoveSet (select-keys (.adds b) [7 8]) {} nil))
        _ (is (= a1 #{1 2 3 4 5 7 8}))
        
        b1 (-> b (cset/remove 7) (cset/remove 4))
        a2 (p79.crdt/join a1
             (or-set/->ObservedRemoveSet {} (select-keys (.removes b1) [7]) nil))
        _ (is (= a2 #{1 2 3 4 5 8}))
        
        b2 (p79.crdt/join b b1)
        _ (is (= b2 #{5 6 8}))
        
        _ (is (= (reduce p79.crdt/join [a2 b b1 b2]) #{1 2 3 4 5 6 8}))
        _ (is (= (reduce p79.crdt/join [a2 b b1 b2 (cset/remove a 4)])
                 #{1 2 3 5 6 8}))]))
