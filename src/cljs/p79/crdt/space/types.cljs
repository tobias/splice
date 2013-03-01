(ns p79.crdt.space.types
  (:require cljs.reader)
  (:require-macros [p79.crdt.space.root-type :refer (defroottype)]))

(defroottype :cljs Entity entity "entity" e string?)