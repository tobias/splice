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
  #+cljs IHash
  (#+clj hashCode #+cljs -hash [this]
    (inc (hash (reference-components this))))
  #+cljs IEquiv
  (#+clj equals #+cljs -equiv [this other]
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

;;; ********************* partially-ordered attribute "names"


; TODO would like to deref to get the attr, but the rank is hardly "just"
; metadata like reference epochs (mostly?) are
(defprotocol IPOAttribute
  (attr [x])
  (rank [x]))

(defn- poattr-components
  [poattr]
  [(attr poattr) (rank poattr)]) 

(deftype POAttribute [attr rank]
  IPOAttribute
  (attr [_] attr)
  (rank [_] rank)
  #+clj Comparable #+cljs IComparable
  (#+clj compareTo #+cljs -compare [_ other]
    (!0 (sedan.impl/default-compare attr (.-attr other))
      (sedan.impl/default-compare rank (.-rank other))))
  #+clj Object
  #+clj
  (toString [this] (pr-str this))
  #+cljs IHash
  (#+clj hashCode #+cljs -hash [this]
    (inc (hash (poattr-components this))))
  #+cljs IEquiv
  (#+clj equals #+cljs -equiv [this other]
    (and (instance? POAttribute other)
      (= attr (.-attr other))
      (= rank (.-rank other)))))

(declare po-attr)

(defn- po-attr-tagged-reader-fn
  [x]
  (assert (and (vector? x) (== 2 (count x)))
    "po-attr must be represented by a vector of [attribute, rank]")
  (apply po-attr x))

; TODO could print rank strings (if they _are_ strings) in hex to make REPL debugging easier

#+clj
(do
  (alter-var-root #'cljs.tagged-literals/*cljs-data-readers*
    assoc 'po po-attr-tagged-reader-fn)

  (defmethod print-method POAttribute [poattr ^java.io.Writer w]
    (.write w "#po ")
    (print-method (poattr-components poattr) w))
  (defmethod print-dup POAttribute [o w]
    (print-method o w))
  (#'clojure.pprint/use-method
    clojure.pprint/simple-dispatch
    POAttribute 
    #'clojure.pprint/pprint-simple-default)

  (defmethod cljsc/emit-constant POAttribute 
    [^POAttribute po-attr]
    (cljsc/emits "cemerick.splice.types.po_attr(")
    (cljsc/emit-constant (.-attr po-attr))
    (cljsc/emits ",")
    (cljsc/emit-constant (.-rank po-attr))
    (cljsc/emits ")")))

#+cljs
(do
  (cljs.reader/register-tag-parser! 'po po-attr-tagged-reader-fn)
  (extend-type POAttribute 
    IPrintWithWriter
    (-pr-writer [this w opts]
      (-write w "#po ")
      (pr-writer (poattr-components this) w opts))))

(defn po-attr?
  "Returns true iff [x] is a partially-ordered attribute."
  [x]
  (instance? POAttribute x))

(defn po-attr
  "Creates a new partially-ordered attribute given a base [attribute] (which may
  be any splice-compatible value _other than_ a po-attr), and a [rank] value
  indicating the ordering of that attribute.

When called with a POAttribute, it will be returned unmodified.  When
called with a POAttribute and a new rank value, will return a POAttribute that
contains the same base attribute, but with the new rank."
  [attr rank]
  (if (instance? POAttribute attr)
    (POAttribute. (.-attr attr) rank)
    (POAttribute. attr rank)))

(define-partition! 0x51 :po-attr POAttribute 
  (fn decode-po-attr [tag ^String s]
    (sedan.impl/decode-sequence po-attr (sedan.impl/without-tag s) false))
  (encode* [^POAttribute x buffer]
    (sedan.impl/encode-sequence [(.-attr x) (.-rank x)] buffer false))
  (compare* [a b]
    (sedan.impl/default-compare a b)))

