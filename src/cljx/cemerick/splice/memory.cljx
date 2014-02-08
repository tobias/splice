(ns cemerick.splice.memory
  (:require [cemerick.splice :as s :refer (TupleStore)]
            [cemerick.splice.memory.query :as q]
            #+clj [cemerick.splice.memory.planning :as p]
            [cemerick.splice.hosty :refer (now)]
            [cemerick.splice.uuid :refer (random-uuid)]
            [clojure.set :as set])
  #+cljs (:require-macros [cemerick.splice.memory.planning :as p]))

(defn- add-tuples
  [indexes tuples]
  (reduce
    (fn [indexes index-keys]
      (update-in indexes [index-keys] into tuples))
    indexes
    (keys indexes)))

(deftype MemSpace [site-id last-write-num metadata indexes]
  #+clj clojure.lang.IMeta
  #+clj (meta [this] metadata)
  #+cljs IMeta
  #+cljs (-meta [this] metadata)
  #+clj clojure.lang.IObj
  #+clj (withMeta [this meta] (MemSpace. site-id last-write-num meta indexes))
  #+cljs IWithMeta
  #+cljs (-with-meta [this meta] (MemSpace. site-id last-write-num meta indexes))
  TupleStore
  (available-indexes [this] (set (keys indexes)))
  (write* [this tuples]
    (let [[write-num tuples] (s/placeholders->eids site-id last-write-num tuples)
          metadata (if (== write-num last-write-num)
                     metadata
                     ; TODO this ::last-write bullshit is (almost) useless,
                     ; should just be a query
                     (assoc metadata ::last-write [site-id write-num]))]
      (MemSpace. site-id write-num metadata (add-tuples indexes tuples))))
  (scan [this index-spec beg end]
    (s/scan this index-spec beg end nil))
  (scan [this index-spec beg end options]
    (let [reverse (:reverse options false)
          index (indexes index-spec)]
      (assert index (str "No index available corresponding to index-spec "
                      index-spec))
      ((if reverse rsubseq subseq)
       index >= beg <= end))))

(defn site-idq
  [space]
  (ffirst (q/q space (p/plan {:select [?site]
                              :where [["-local-config" :db/-site-id ?site]]}))))

(defn in-memory
  ([]
     (let [site-id (random-uuid)
           [_ tuples] (s/prep-write nil [{:db/eid "-local-config"
                                          :db/-site-id site-id}])]
       (s/write* (MemSpace. site-id 0 {} q/empty-indexes) tuples)))
  ([init-tuples]
     (let [indexes (add-tuples q/empty-indexes init-tuples)
           temp (MemSpace. nil nil nil indexes)
           site-id (site-idq temp)
           ; TODO this sort of answer should be queryable; we're really just
           ; looking for the first (last) result of a sorted set and
           ; destructuring; seems within reach once results are delivered as a
           ; lazy seq and not a concrete set
           last-write-num (->> (s/scan temp [:write :a :e :v :remove-write]
                                 (s/tuple s/index-bottom s/index-bottom s/index-bottom
                                   [site-id s/index-bottom])
                                 (s/tuple s/index-top s/index-top s/index-top 
                                   [site-id s/index-top])
                                 {:reverse true})
                            first
                            :write
                            second)]
       (assert (number? last-write-num) "Could not find last write number in initial set of tuples for in-memory splice")
       (MemSpace. site-id last-write-num {} indexes))))

;; new write ++
;; new write with metadata ++
;; replicated write (implies additional new write w/ repl metadata ++)






















