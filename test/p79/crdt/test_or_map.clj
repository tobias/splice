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
  (let [maps (vec (take 3 (repeatedly #(ref (create)))))
        run? (atom true)
        twiddle-some-set #(dosync
                            (apply commute (first maps)
                                     (let [key (rand-nth [:a :b :c :d :e :f :g :h :i
                                                :j :k :l :m :n :p :q :r :s])]
                                       (if (>= 25 (rand-int 100))
                                       [map/add key (rand-int 20)]
                                       [map/remove key]))))
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
                                          maps)]
                       (while @run?
                         (Thread/sleep 200)
                         (x-replicate))
                       (x-replicate)))]
    (def maps maps)
    
    (Thread/sleep 5000)
    (reset! run? false)
    @replicator  ;; ensure replication completes
    
    (println "After replication:" (map (comp seq deref) maps))
    (is (apply = (map deref maps)))
    (is (apply = [] (map (comp crdt/log deref) (butlast maps))))
    (is (pos? (count (crdt/log @(last maps)))))))
