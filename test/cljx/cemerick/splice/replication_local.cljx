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
                          [cemerick.cljs.test :refer (deftest is are block-or-done
                                                       run-tests done with-test-ctx)]
                          [cljs.core.async.macros :refer (go)])
  #+clj (:use clojure.test))

(deftest ^:async simplest
  (let [a (atom (in-memory))
        b (atom (in-memory))
        c (atom (in-memory))
        ctrl (rep/peering-replication a b)
        ctrl' (rep/peering-replication b c)
        query (plan {:select [?k ?v ?write]
                     :where [["foo" ?k ?v ?write]]})]
    (swap! a s/write [{:a 5 ::s/e "foo" :b 6}])
    ; TODO how to *actually* monitor replication?
    (block-or-done
     (go (<! (async/timeout 500))
         (is (= (set (q @a query))
               (set (q @b query))
               (set (q @c query))))

         (let [replicated-write (-> @a meta ::mem/last-write)]
           (is (:local/replicated (mem/entity-map @b replicated-write)))
           (is (:local/replicated (mem/entity-map @c replicated-write))))
         
         ; cancel replication
         (>! ctrl true)
         (>! ctrl' true)

         (let [last-write (-> @a meta ::mem/last-write)
               bar-query (plan {:select [?c]
                                :where [[_ :c ?c]]})]
           (swap! a s/write [{:c "bar" ::s/e "foo"}])
           (is (= [["bar"]] (q @a bar-query)))
           (<! (async/timeout 500))
           (is (empty? (q @b bar-query)))
           (is (empty? (q @c bar-query)))
           (is (= last-write (:last-write (<! ctrl))))
           (is (= last-write (:last-write (<! ctrl')))))))))

(deftest ^:async never-replicate-local-attrs
  (let [a (atom (in-memory))
        b (atom (in-memory))
        last-write (::mem/last-write (meta @b))
        ctrl (rep/peering-replication a b)
        query (plan {:select [?k ?v ?write]
                     :where [["foo" ?k ?v ?write]]})]
    (swap! a s/write [{:local/a 5 ::s/e "foo"}])
    ; TODO how to *actually* monitor replication?
    (block-or-done
     (go (<! (async/timeout 500))
       (is (= last-write (::mem/last-write (meta @b))))

       (swap! a s/write [{'local/a 6 :a 7 ::s/e "foo"}])

       (<! (async/timeout 500))
       (is (not= last-write (::mem/last-write (meta @b))))
       (let [query (plan {:select [?a]
                          ; TODO symbols passed to plan can't be quoted, this is wrong
                          :where #{[["foo" local/a ?a]]
                                   [["foo" :local/a ?a]]}})]
         (is (= [[7 (::mem/last-write (meta @a))]]
               (q @b (plan {:select [?a ?w]
                            :where [["foo" :a ?a ?w]]}))))
         (is (= #{5 6} (->> (q @a query) (apply concat) set)))
         (is (empty? (q @b query))))))))

(deftest ^:async replicate-removals
  (let [a (atom (in-memory))
        b (atom (in-memory))
        ctrl (rep/peering-replication a b)
        query (plan {:select [?k ?v]
                     :where [["foo" ?k ?v]]})]
    (swap! a s/write [{:a 5 ::s/e "foo"}])
    (is (= [[:a 5]] (q @a query)))

    (block-or-done
      (go (<! (async/timeout 500))
        (is (= [[:a 5]] (q @a query)))
        (swap! a s/write [["foo" :a 5 nil (-> @a meta ::mem/last-write)]])
        (is (= [] (q @a query)))
        (<! (async/timeout 500))
        (is (= [] (q @b query)))))))

(deftest ^:async simplest-watcher
  (let [src (atom (in-memory))
        tgt (atom (in-memory))
        ctrl (rep/peering-replication src tgt)
        query (plan {:select [?k ?v ?write]
                     :where [["foo" ?k ?v ?write]]})
        complete (async/chan)]
    (swap! src s/write [{:a 5 ::s/e "foo" :b 6}])
    ; TODO how to *actually* monitor replication?
    (add-watch tgt :repl
               (fn [_ _ _ ts]
                 (when (= (set (q @src query)) (set (q @tgt query)))
                   (remove-watch tgt :repl)
                   (async/close! ctrl)
                   (async/put! complete true))))
    (block-or-done
     (go (is (first (alts! [complete (async/timeout 5000)])))))))
