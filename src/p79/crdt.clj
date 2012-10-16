(ns p79.crdt)

(defprotocol Joinable
  (join [a b]))
