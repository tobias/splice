(ns p79.crdt.vclock-test
  (:require [p79.crdt.vclock :as vc])
  (:use clojure.test))

(deftest test-descends
  (testing "all vclocks descend from the empty vclock"
    (is (vc/descendant? (vc/clock) (vc/clock)))))

(deftest sanity
  (let [a0 (vc/clock)
        b0 (vc/clock)
        a1 (vc/tick a0 :a)
        b1 (vc/tick b0 :b)
        a2 (vc/tick a1 :a)
        b2 (vc/tick b1 :b)
        c0 (vc/join a2 b1)
        c1 (vc/tick c0 :c)]
    (is (empty? a0))
    (is (empty? b0))
    (is (vc/descendant? a1 a0))
    (is (vc/descendant? b1 b0))
    (is (not (vc/descendant? a1 b1)))
    (is (vc/descendant? a2 a1))
    (is (vc/descendant? b2 b1))
    (is (not (vc/descendant? a2 b2)))
    (is (vc/descendant? c1 a2))
    (is (vc/descendant? c1 b1))
    (is (not (vc/descendant? b1 c1)))
    (is (not (vc/descendant? b1 a1)))))

(defn- commutatively-concurrent?
  [c1 c2]
  (is (= 0 (vc/compare-clocks c1 c2) (vc/compare-clocks c2 c1)))
  (is (vc/concurrent? c1 c2))
  (is (vc/concurrent? c2 c1))) 

(deftest test-joining
  (let [a (vc/clock)
        a-a (vc/tick a)
        a-b (vc/tick a-a)
        a-c (vc/tick a-b "other")
        a-d (vc/tick a-c "other")
        b-b (vc/tick a-a "other2")
        b-c (vc/tick b-b "other2")
        c (vc/join a-c b-b)
        d (vc/join b-b a-c)]
    (doseq [lineage [[a a-a a-b a-c c]
                     [a a-a a-b a-c d]
                     [a a-a b-b c]
                     [a a-a b-b d]]]
      (is (= lineage (sort vc/compare-clocks (shuffle lineage))))
      (doseq [[i clock] (map-indexed vector lineage)
              [j clock2] (map-indexed vector lineage)
              :let [preds (get {0 [vc/descendant? vc/descendant?]
                                -1 [vc/antecedent? vc/descendant?]
                                1 [vc/descendant? vc/antecedent?]}
                               (compare i j))]]
        (when-not (zero? (compare i j))
          (is (= (compare i j) (vc/compare-clocks clock clock2))))
        (is ((first preds) clock clock2))
        (is ((second preds) clock2 clock))))
    
    (doseq [c1 [a-b a-c a-d]
            c2 [b-b b-c]]
      (commutatively-concurrent? c1 c2))
    (commutatively-concurrent? a-d c)
    (commutatively-concurrent? a-d d)
    (commutatively-concurrent? b-c c)
    (commutatively-concurrent? b-c d)
    
    (is (= c d (vc/join c c)))
    (is (= 1 (vc/compare-clocks c d) (vc/compare-clocks d c)))))
