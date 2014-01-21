(ns cemerick.splice.types
  #+cljs (:require cljs.reader)
  (#+clj :require #+cljs :require-macros
         [cemerick.splice.root-type :refer (defroottype)]))

(def entity identity)
#_#_
(defroottype #+clj :clj #+cljs :cljs Entity entity "entity" e string?)
(defroottype #+clj :clj #+cljs :cljs Ref reference "ref" e entity?)

