; This Source Code Form is subject to the terms of the Mozilla Public License,
; v. 2.0. If a copy of the MPL was not distributed with this file, You can
; obtain one at http://mozilla.org/MPL/2.0/.

(ns cemerick.splice.memory.indexing
  (:require [cemerick.cljs.macro :refer (defportable *cljs* *clj*)]))

(defmacro -index-comparator* [t t2 [k & tuple-keys]]
  (if-not k
    0
    `(let [x# (quilt.sedan/compare (~k ~t) (~k ~t2))]
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
    ; using a fn with metadata as a comparator in sets doesn't work in CLJS w/ :advanced
    ; TODO file ticket/fix
    `(identity ;~'with-meta
       (~'fn [t# t2#]
         (-index-comparator* t# t2# ~tuple-keys))
       #_{:index-keys ~tuple-keys})))
