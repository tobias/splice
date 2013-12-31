(ns cemerick.splice.types
  #+cljs (:require cljs.reader)
  (#+clj :require #+cljs :require-macros
         [cemerick.splice.root-type :refer (defroottype)]))

(defroottype #+clj :clj #+cljs :cljs Entity entity "entity" e string?)
; TODO need to change this to accept an entity OR a vector of entity + tag/write
(defroottype #+clj :clj #+cljs :cljs Ref reference "ref" e entity?)

