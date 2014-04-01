(ns cemerick.splice.types
  (:require [cemerick.sedan :as sedan]
            [cemerick.sedan.impl :as sedan.impl]
            #+cljs cljs.reader
            #+clj [cljs.compiler :as cljsc]
            #+clj cljs.tagged-literals
            #+clj clojure.pprint)
  (#+clj :require #+cljs :require-macros
    [cemerick.sedan.macros :refer (define-partition! !0)]))

; TODO probably should move this to splice.reference; I really don't ever want
; to implement another type like this...

(defprotocol IEpoch
  (as-of [x])
  (unbounded? [x]))

(defn- reference-components
  [reference]
  (if (unbounded? reference)
    @reference
    [@reference (as-of reference)]))

(defn- hash-reference
  [ref]
  (inc (hash (reference-components ref))))

(deftype Reference [referent epoch]
  #+clj clojure.lang.IDeref #+cljs IDeref
  (#+clj deref #+cljs -deref [_] referent)
  IEpoch
  (as-of [x] epoch)
  (unbounded? [x] (= epoch sedan/top))
  #+clj Comparable #+cljs IComparable
  (#+clj compareTo #+cljs -compare [_ other]
    (!0 (sedan.impl/default-compare referent (.-referent other))
      (sedan.impl/default-compare epoch (.-epoch other))))
  #+clj Object
  #+clj
  (toString [this] (pr-str this))
  #+clj
  (hashCode [this] (hash-reference this))
  (equals [this other]
    (and (instance? Reference other)
      (= referent (.-referent other))
      (= epoch (.-epoch other)))))

(declare reference)

(defn- reference-tagged-reader-fn
  [x]
  (if (vector? x)
    (apply reference x)
    (reference x)))

#+clj
(do
  (alter-var-root #'cljs.tagged-literals/*cljs-data-readers*
    assoc 'ref reference-tagged-reader-fn)

  (defmethod print-method Reference [ref ^java.io.Writer w]
    (.write w "#ref ")
    (print-method (if (unbounded? ref)
                    @ref
                    (reference-components ref)) w))
  (defmethod print-dup Reference [o w]
    (print-method o w))
  (#'clojure.pprint/use-method
    clojure.pprint/simple-dispatch
    Reference
    #'clojure.pprint/pprint-simple-default)

  (defmethod cljsc/emit-constant Reference
    [^Reference ref]
    (cljsc/emits "cemerick.splice.types.reference(")
    (cljsc/emit-constant @ref)
    (when-not (unbounded? ref)
      (cljsc/emits ",")
      (cljsc/emit-constant (.-epoch ref)))
    (cljsc/emits ")")))

#+cljs
(do
  (cljs.reader/register-tag-parser! 'ref reference-tagged-reader-fn)
  (extend-type Reference
    IHash
    (-hash [this] (hash-reference this))
    IEquiv
    (-equiv [this other]
      (and (instance? Reference other)
        (= (.-referent this) (.-referent other))
        (= (.-epoch this) (.-epoch other))))
    IPrintWithWriter
    (-pr-writer [this w opts]
      (-write w "#ref ")
      (pr-writer (if (unbounded? this)
                    @this
                    (reference-components this))
        w opts))))

(defn reference?
  "Returns true iff [x] is a Reference."
  [x]
  (instance? Reference x))

(defn reference
  "Creates a new reference given a [referent], and an optional value indicating
  the endpoint of the epoch after which changes to the [referent] should be
  disregarded.

When called with a Reference, that Reference will be returned unmodified.  When
called with a Reference and an epoch endpoint, will return a Reference that
contains the same referent, but with the new epoch endpoint."
  ([referent]
     (if (instance? Reference referent)
       referent
       (reference referent sedan/top)))
  ([referent epoch]
     (if (instance? Reference referent)
       (Reference. (.-referent referent) epoch)
       (Reference. referent epoch))))

(define-partition! 0x50 :reference Reference
  (fn decode-reference [tag ^String s]
    (sedan.impl/decode-sequence reference (sedan.impl/without-tag s) false))
  (encode* [^Reference x buffer]
    (sedan.impl/encode-sequence [(.-referent x) (.-epoch x)] buffer false))
  (compare* [a b]
    (sedan.impl/default-compare a b)))
