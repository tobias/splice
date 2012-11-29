(ns p79.crdt.map
  (:refer-clojure :exclude (remove)))

(defprotocol Map
  (add [this k v] [this k v tags])
  (remove [this k] [this k tags])
  (lookup [this k]))