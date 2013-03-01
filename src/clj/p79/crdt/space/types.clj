(ns p79.crdt.space.types
  (:require [p79.crdt.space.root-type :refer (defroottype)]))

(defroottype :clj Entity entity "entity" e string?)