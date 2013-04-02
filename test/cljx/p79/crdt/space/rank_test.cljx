(ns p79.crdt.space.rank-test
  (:require [p79.crdt.space.rank :refer (rank between) :as rank]
            [p79.math :as math]
            [cemerick.cljs.test :as t])
  ^:clj (:use clojure.test)
  ^:cljs (:require-macros [cemerick.cljs.test :refer (deftest is are)]))

(def rank-compare ^:clj #'rank/rank-compare ^:cljs rank/rank-compare)

(deftest definitional
  (are [compare-result r1 r2] (and (== compare-result (rank-compare r1 r2))
                                  (== (- compare-result) (rank-compare r2 r1)))
    0 [] []
    0 [] [0]
    
    0 [42] [42]
    
    -1 [] [2]
    1 [2] []
    1 [2] [-2]
    
    1 [1 2 3] [1 2 1]
    -1 [1 2 -3] [1 2 1]
    
    1 [1 2 3] [1 2 0]
    0 [1 2 3] [1 2 3 0]
    1 [1 2 3] [1 2 3 -1]
    -1 [1 2 3] [1 2 3 1]))

(deftest equality
  (is (= (rank [1 2]) (rank [1 2])))
  (is (not= (rank [1 2]) (rank [1 1])))
  (is (not= (rank [1 2]) 5)))

(deftest explicit-between
  ^:cljs (is (thrown? js/Error (rank/between* [] [])))
  ^:clj (is (thrown? AssertionError (rank/between* [] [])))
  ^:cljs (is (thrown? js/Error (rank/between* [1 2 3] [1 2 3])))
  ^:clj (is (thrown? AssertionError (rank/between* [1 2 3] [1 2 3])))
  (are [r1 r2 between] (and (= (rank/between* r1 r2)
                              (rank/between* r2 r1)
                              between)
                         (let [[low high] (if (== -1 (rank-compare r1 r2))
                                            [r1 r2]
                                            [r2 r1])]
                           (== -1 (rank-compare between high))
                           (== 1 (rank-compare high between))
                           (== -1 (rank-compare low between))
                           (== 1 (rank-compare between low))))
    [] rank/HIGH [8.988465674311579E307]
    rank/LOW [] [-8.988465674311579E307]
    rank/LOW rank/HIGH (conj rank/LOW 1)
    
    [(- math/MIN)] [math/MIN] [0.0]
    [(- math/MIN)] [] [(- math/MIN) 1]
    
    [] [math/MIN] [0 1]
    
    [0] [1] [0.5]
    [-1] [1] [0.0]
    [1 2] [1 2 3] [1 2 1.5]
    [] [1] [0.5]
    [1 2] [1 1 1 1 2] [1 1.5]
    
    ))
