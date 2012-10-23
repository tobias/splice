(ns p79.crdt.test-or-set
  (:require [p79.crdt :as crdt]
            [p79.crdt.set :as cset]
            [p79.crdt.or-set :refer (create)]
            [p79.vclock :as vc])
  (:use clojure.test)
  (:refer-clojure :exclude (replicate)))

(deftest sanity
  (let [a (create 1 2 3 4 5)
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

(defn- apply-log
  [log set]
  (dosync
    (commute set #(reduce
                    (partial apply crdt/update)
                    %
                    log))))


(defn- replicate
  [from to]
  (dosync
    (let [log (crdt/log (ensure from))]
      ;(alter from crdt/truncate-log)
      (commute to #(reduce
                     (partial apply crdt/update)
                     %
                     log)))))

(deftest one-way-transitive-replication
  (let [sets (vec (take 3 (repeatedly #(ref (create)))))
        run? (atom true)
        twiddle-some-set #(dosync
                            (commute (first sets)
                                     (if (>= 25 (rand-int 100))
                                       cset/add
                                       cset/remove)
                                     (rand-int 20)))
        setters (doall (take 3 (repeatedly
                                 #(future (binding [vc/nodename (str "n" (swap! @#'vc/counter inc))]
                                            (while @run? (twiddle-some-set)))))))
        replicator (future
                     (let [x-replicate #(reduce
                                          (fn [from to]
                                            (dosync
                                              (let [log (crdt/log (ensure from))]
                                                (alter from crdt/truncate-log)
                                                (apply-log log to)))
                                            to)
                                          sets)]
                       (while @run?
                         (Thread/sleep 200)
                         (x-replicate))
                       (x-replicate)))]
    ;(def sets sets)
    
    (Thread/sleep 5000)
    (reset! run? false)
    @replicator  ;; ensure replication completes
    
    (println "After replication:" (map (comp seq deref) sets))
    (is (apply = (map deref sets)))
    (is (apply = [] (map (comp crdt/log deref) (butlast sets))))
    (is (pos? (count (crdt/log @(last sets)))))
    
    (is (= (crdt/join (create) @(last sets))))))
