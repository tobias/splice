(ns cemerick.splice.memory.indexing
  (:require [cemerick.cljs.macro :refer (defportable *cljs* *clj*)]))

(defmacro -index-comparator* [t t2 [k & tuple-keys]]
  (if-not k
    0
    `(let [x# (cemerick.sedan/compare (~k ~t) (~k ~t2))]
       (if (zero? x#)
         (-index-comparator* ~t ~t2 ~tuple-keys)
         x#))))

(defportable index-comparator
  [tuple-keys]
  (if *clj*
    `(with-meta
       (reify java.util.Comparator
         (compare [_# t# t2#]
           (-index-comparator* t# t2# ~tuple-keys)))
       {:index-keys ~tuple-keys})
    `(~'with-meta
       (~'fn [t# t2#]
         (-index-comparator* t# t2# ~tuple-keys))
       {:index-keys ~tuple-keys})))
