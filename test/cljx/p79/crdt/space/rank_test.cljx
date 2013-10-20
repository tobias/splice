(ns p79.crdt.space.rank-test
  (:require [p79.crdt.space.rank :refer (rank between) :as rank]
            [p79.math :as math]
            [cemerick.cljs.test :as t])
  #+clj (:use clojure.test)
  #+cljs (:require-macros [cemerick.cljs.test :refer (deftest is are)]))

(def rank-compare #+clj #'rank/rank-compare #+cljs rank/rank-compare)

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
  #+cljs (is (thrown? js/Error (rank/between* [] [])))
  #+clj (is (thrown? AssertionError (rank/between* [] [])))
  #+cljs (is (thrown? js/Error (rank/between* [1 2 3] [1 2 3])))
  #+clj (is (thrown? AssertionError (rank/between* [1 2 3] [1 2 3])))
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

#_#_#_#_#_
(def generated-cnt 1000)
(def between-cnt 50)

(deftest random-between-generation
  (doseq [x (repeatedly generated-cnt rand)
            :when (pos? x)
            :let [seed (rank x)
                  anchor (rand-nth [0 1])
                  [comp-fn comparison-result] (if (> x anchor)
                                                [rank/rank> 1]
                                                [rank/rank< -1])
                  anchor (rank anchor)
                  ranks (take between-cnt (iterate (partial between anchor) seed))
                  ranks (concat ranks [anchor])]]
    (is (apply comp-fn ranks) [seed anchor comp-fn])
    (doseq [pair (partition 2 1 ranks)]
      (is (== comparison-result (apply compare pair)) pair))))

(defn- random-double
  "Generates a random double in the range [-1e-200 1e200]"
  []
  (* (rand) (Math/pow 10 (- (rand-int 400) 200))))

(deftest comparisons
  (doseq [numbers (->> (repeatedly random-double)
                    (partition 2)
                    (take generated-cnt))
          :let [ranks (map rank numbers)
                ncomp (apply compare numbers)]]
    (is (= ncomp (apply compare ranks)) (str numbers ranks))
    (let [comp-fn (cond
                    (pos? ncomp) rank/rank>
                    (neg? ncomp) rank/rank<
                    :default =)]
      (is (apply comp-fn ranks)))))
