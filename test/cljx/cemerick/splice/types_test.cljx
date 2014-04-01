(ns cemerick.splice.types-test
  (:require [cemerick.sedan :as sedan]
            cemerick.splice
            [cemerick.splice.rank :as r]
            [cemerick.splice.types :refer (reference)]
            cemerick.splice.rank-test
            #+cljs [cljs.reader :refer (read-string)]
            #+clj [clojure.test :refer :all :as t]
            #+cljs [cemerick.cljs.test :as t]
                        cemerick.double-check
            #+clj [cemerick.double-check.clojure-test :as qc]
            [cemerick.double-check.properties #+clj :refer #+clj (for-all)]
            [cemerick.double-check.clojure-test.runtime :as qcrt]
            [cemerick.double-check.generators :as gen])
  #+cljs (:require-macros [cemerick.double-check.properties :refer (for-all)]
                          [cemerick.double-check.clojure-test :as qc]
                          [cemerick.cljs.test :refer (deftest is run-tests)]))

(deftest references
  (let [j #ref "foo"
        k (reference "foo")
        l (read-string (pr-str k))]
    (is (= j k l))
    (is (every? zero? (map compare [j k l] [l j k])))
    (is (every? #(= "foo" %) (map deref [j k l])))
    (is (every? #(= "foo" (.-referent %)) [j k l]))
    (is (every? #(= (hash j) (hash %)) [j k l]))))

; TODO totally a placeholder; need to factor all of the generators that sedan
; uses in testing into their own library, use here
(def ^:private gen-sedan-value gen/string)

(def ^:private gen-reference
  (gen/fmap (partial apply reference)
    (gen/tuple gen-sedan-value (gen/frequency [[1 gen-sedan-value]
                                               [2 (gen/return sedan/top)]]))))

; no easy way to roll this into the existing (intensive) sedan tests :-/
; TODO could just push Reference down into sedan; just how much does that make
; sedan a library for quilt, as opposed to something generally useful?  And, do
; we care?
(qc/defspec reference-sedan-spec cemerick.splice.rank-test/number-trials
  ; this is basically a rewrite of the base sedan spec :-/
  (for-all [ref gen-reference
            ref' gen-reference]
    (let [[enc enc' :as encoded] (map sedan/encode [ref ref'])]
      (and (= [0x50 0x50] (map #(#+clj #'r/char-code #+cljs r/char-code % 0) encoded))
        (= (r/sign (compare ref ref')) (r/sign (compare enc enc')))
        (= (r/sign (compare ref' ref)) (r/sign (compare enc' enc)))
        (= ref (sedan/decode enc))
        (= ref' (sedan/decode enc'))))))

