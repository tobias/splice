(ns p79.crdt.set
  (:refer-clojure :exclude (remove)))

(defprotocol Set
  (add [this e] [this e tag])
  (remove [this e] [this e tags])
  (contains [this e]))