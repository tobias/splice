(ns p79.vclock
  (:require [clojure.set :as set])
  (:refer-clojure :exclude (time)))

;; TODO: pruning of some sort
;; (or, implement interval tree clocks)

; implemented after glancing at many vclock impls, including
; https://github.com/voldemort/voldemort/blob/master/src/java/voldemort/versioning/VectorClock.java
; https://github.com/michaelklishin/vclock

(defn- uuid [] (java.util.UUID/randomUUID))
;#+cljs (defn- uuid [] (UUID.))

(defonce ^:dynamic nodename (uuid))
(defonce ^:private counter (atom 0))

(defonce ^{:doc "reset! to have timestamps on vector clock entries adjusted by some amount
(very handy for ~synchronizing client timestamps with server timestamps)"}
  timestamp-adjustment (atom 0))

(defn- time
  ([] (time @timestamp-adjustment))
  ([timestamp-adjustment]
    (+ timestamp-adjustment (System/currentTimeMillis))))

(defn entry
  ([] (entry nodename))
  ([nodename] (entry nodename (swap! counter inc) (time)))
  ([nodename count t] {:name nodename :counter count :t t}))

(defn clock
  [& entries]
  (into {} (for [e entries] [(:name e) e])))

(defn tick
  ([vclock] (tick vclock nodename))
  ([vclock nodename]
    (update-in vclock [nodename] #(if %
                                    (assoc-in % [:counter] (swap! counter inc))
                                    (entry nodename)))))

(defn join
  [& clocks]
  (apply merge-with
         #(or (and (> (:counter %) (:counter %2)) %) %2)
         clocks))

(defn- compare-descendant?
  [a b]
  (and (>= (count a) (count b))
       (every?
         #(when-let [a-entry (get a (:name %))]
            (>= (:counter a-entry) (:counter %)))
         (vals b))))

(defn compare-clocks
  "Returns -1, 0, or 1 if clock [a] is antecedent, concurrent/independent,
or descendant of clock [b].

A vclock is its own descendant.
Any vclock is a descendant of an empty vclock."
  [a b]
  (cond
    (and (empty? a) (empty? b)) 1
    (empty? a) -1
    (empty? b) 1
    (= a b) 1
    (compare-descendant? a b) 1
    (compare-descendant? b a) -1
    :else 0))

(def ^{:doc "Returns true if clock [a] is a descendant of clock [b].
A vclock is its own descendant.
Any vclock is a descendant of an empty vclock."
       :arglists '([a b])} descendant? (comp pos? compare-clocks))
(def ^{:doc "Returns true if clock [a] is antecedent of clock [b]."
       :arglists '([a b])} antecedent? (comp neg? compare-clocks))
(def ^{:doc "Returns true if clock [a] is neither descendant nor
antecedent of clock [b]."
       :arglists '([a b])} concurrent? (comp zero? compare-clocks))


