(ns cemerick.splice.rank-test
  (:require [cemerick.splice.rank :refer (rank between) :as rank]
            [cemerick.splice.math :as math]
            [cemerick.cljs.test :as t]
            clojure.test.check
            #+clj [clojure.test.check.clojure-test :as qc]
            [clojure.test.check.properties #+clj :refer #+clj (for-all)]
            [clojure.test.check.clojure-test.runtime :as qcrt]
            [clojure.test.check.generators :as gen])
  #+clj (:use clojure.test)
  #+cljs (:require-macros [clojure.test.check.properties :refer (for-all)]
                          [clojure.test.check.clojure-test :as qc]
                          [cemerick.cljs.test :refer (deftest are is)]))

#+clj (set! *warn-on-reflection* true)

#+clj (.doReset #'qcrt/*report-trials* qcrt/trial-report-periodic)
#+clj (.doReset #'qcrt/*report-shrinking* true)
#+cljs (set! qcrt/*report-trials* qcrt/trial-report-periodic)
#+cljs (set! qcrt/*report-shrinking* true)

(def number-trials
  #+clj (Long/parseLong (System/getProperty "quickcheck-times" "10000"))
  #+cljs (js/parseInt (or (this-as this (aget this "quickcheck_times")) "10000")))

(deftest range-contracts
  (is (thrown? #+clj Exception #+cljs js/Error (rank/before (rank rank/LOW))))
  (is (thrown? #+clj Exception #+cljs js/Error (rank/after (rank rank/HIGH))))
  (is (thrown? #+clj Exception #+cljs js/Error (rank/before (rank ""))))
  (is (thrown? #+clj Exception #+cljs js/Error (rank/after (rank ""))))
  (is (thrown? #+clj Exception #+cljs js/Error (rank/between (rank rank/HIGH) (rank rank/LOW))))
  (is (thrown? #+clj Exception #+cljs js/Error (rank/between (rank "a") (rank "a"))))
  (is (thrown? #+clj Exception #+cljs js/Error (rank/after (rank [(inc rank/high-code)]))))

  (is (= "foo" (str (rank "foo")))))

(def gen-rank-code-points (gen/choose (inc rank/low-code) (dec rank/high-code)))

(def gen-rank (gen/fmap rank (gen/such-that seq (gen/vector gen-rank-code-points))))

(defn- in-order?
  [ranks]
  (boolean (reduce
            (fn [r1 r2]
              (let [s (rank/sign (compare r1 r2))]
                (if (and (neg? s) (== s (- (rank/sign (compare r2 r1)))))
                 r2
                 (reduced false))))
            (first ranks)
            (rest ranks))))

(defn- rank-spec
  [start]
  (let [before (rank/before start)
        after (rank/after start)
        between-low (rank/between (rank rank/LOW) start)
        between-high (rank/between start (rank rank/HIGH))]
    (and (in-order? [before start after])
         (in-order? [between-low start between-high]))))

(defn- rank-between-spec
  [a b]
  (let [between (rank/between a b)]
    (let [[a b] (if (pos? (compare a b))
                  [b a] [a b])]
      (in-order? [a between b]))))

(qc/defspec rank-basics number-trials
  (for-all [a gen-rank b gen-rank]
           (or (= a b)
               (and (rank-spec a)
                    (rank-spec b)
                    (rank-between-spec a b)))))
