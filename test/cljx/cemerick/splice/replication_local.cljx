(ns cemerick.splice.replication-local
  (:require [cemerick.splice :as s :refer (write)]
            [cemerick.splice.memory.query :refer (q)]
            [cemerick.splice.replication :as rep]
            [cemerick.splice.memory :as mem :refer (in-memory)]
            #+clj [cemerick.splice.memory.planning :refer (plan)]
            #+clj [clojure.core.async :as async :refer (go go-loop >! <! alts!)]
            #+cljs [cljs.core.async :as async :refer (>! <! alts!)]
            [cemerick.cljs.test :as t :refer (#+clj block-or-done)])
  #+cljs (:require-macros [cemerick.splice.memory.planning :refer (plan)]
                          [cemerick.cljs.test :refer (deftest is are block-or-done run-tests done with-test-ctx)]
                          [cljs.core.async.macros :refer (go)])
  #+clj (:use clojure.test))

(deftest ^:async simplest
  (let [src (atom (in-memory))
        tgt (atom (in-memory))
        ctrl (rep/peering-replication src tgt)
        query (plan {:select [?k ?v ?write]
                     :where [["foo" ?k ?v ?write]]})]
    (swap! src s/write [{:a 5 :db/eid "foo" :b 6}])
    ; TODO how to *actually* monitor replication?
    (block-or-done
     (go (<! (async/timeout 500))
         (let [query (plan {:select [?k ?v ?write]
                            :where [["foo" ?k ?v ?write]]})]
           (is (= (q @src query) (q @tgt query))))
         (async/close! ctrl)))))

(deftest ^:async simplest-watcher
  (let [src (atom (in-memory))
        tgt (atom (in-memory))
        ctrl (rep/peering-replication src tgt)
        query (plan {:select [?k ?v ?write]
                     :where [["foo" ?k ?v ?write]]})
        complete (async/chan)]
    (swap! src s/write [{:a 5 :db/eid "foo" :b 6}])
    ; TODO how to *actually* monitor replication?
    (add-watch tgt :repl
               (fn [_ _ _ ts]
                 (when (= (q @src query) (q @tgt query))
                   (remove-watch tgt :repl)
                   (async/close! ctrl)
                   (async/put! complete true))))
    (block-or-done
     (go (is (first (alts! [complete (async/timeout 5000)])))))))
