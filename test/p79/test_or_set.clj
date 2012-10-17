(ns p79.test-or-set
  (:require [p79.crdt.or-set :refer (create)])
  (:use clojure.test))

(deftest sanity
  (let [a (create 1 2 3 4 5)
        b (disj a 2)
        c (disj b 4)
        d (conj c 6)
        e (conj a 6)
        f (disj b 4)]
    (is (= #{1 2 3 4 5} a))
    (is (= #{1 3 4 5} b))
    (is (= #{1 3 5} c))
    (is (= #{1 3 5 6} d))
    (is (= #{1 2 3 4 5 6} e))
    (is (= #{1 3 5} f))
    (is (= #{6 5 3 1} (p79.crdt/join e d)))))

