(ns cemerick.splice.types
  (:require [quilt.sedan :as sedan]
            [quilt.sedan.impl :as sedan.impl]
            #+cljs cljs.reader
            #+clj [cljs.compiler :as cljsc]
            #+clj cljs.tagged-literals
            #+clj clojure.pprint)
  (#+clj :require #+cljs :require-macros
    [quilt.sedan.macros :refer (define-partition! !0)]))

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


;;; ********************* (partially) ordered attributes

; TODO would like to deref to get the attr, but the rank is hardly "just"
; metadata like reference epochs (mostly?) are
(defprotocol IOrderedAttribute
  (attr [x])
  (rank [x]))

(defn- oattr-components
  [oattr]
  [(attr oattr) (rank oattr)]) 

(deftype OrderedAttribute [attr rank]
  IOrderedAttribute
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
    (inc (hash (oattr-components this))))
  #+cljs IEquiv
  (#+clj equals #+cljs -equiv [this other]
    (and (instance? OrderedAttribute other)
      (= attr (.-attr other))
      (= rank (.-rank other)))))

(declare oattr)

(defn- oattr-tagged-reader-fn
  [x]
  (assert (and (vector? x) (== 2 (count x)))
    "oattr must be represented by a vector of [attribute, rank]")
  (apply oattr x))

; TODO could print rank strings (if they _are_ strings) in hex to make REPL debugging easier

#+clj
(do
  (alter-var-root #'cljs.tagged-literals/*cljs-data-readers*
    assoc 'oa oattr-tagged-reader-fn)

  (defmethod print-method OrderedAttribute [oattr ^java.io.Writer w]
    (.write w "#oa ")
    (print-method (oattr-components oattr) w))
  (defmethod print-dup OrderedAttribute [o w]
    (print-method o w))
  (#'clojure.pprint/use-method
    clojure.pprint/simple-dispatch
    OrderedAttribute 
    #'clojure.pprint/pprint-simple-default)

  (defmethod cljsc/emit-constant OrderedAttribute 
    [^OrderedAttribute oattr]
    (cljsc/emits "cemerick.splice.types.oattr(")
    (cljsc/emit-constant (.-attr oattr))
    (cljsc/emits ",")
    (cljsc/emit-constant (.-rank oattr))
    (cljsc/emits ")")))

#+cljs
(do
  (cljs.reader/register-tag-parser! 'oa oattr-tagged-reader-fn)
  (extend-type OrderedAttribute 
    IPrintWithWriter
    (-pr-writer [this w opts]
      (-write w "#oa ")
      (pr-writer (oattr-components this) w opts))))

(defn oattr?
  "Returns true iff [x] is a partially-ordered attribute."
  [x]
  (instance? OrderedAttribute x))

(defn oattr
  "Creates a new partially-ordered attribute given a base [attribute] (which may
  be any splice-compatible value _other than_ a oattr), and a [rank] value
  indicating the ordering of that attribute.

When called with a OrderedAttribute, it will be returned unmodified.  When
called with a OrderedAttribute and a new rank value, will return a OrderedAttribute that
contains the same base attribute, but with the new rank."
  [attr rank]
  (if (instance? OrderedAttribute attr)
    (OrderedAttribute. (.-attr attr) rank)
    (OrderedAttribute. attr rank)))

(define-partition! 0x51 :oattr OrderedAttribute 
  (fn decode-oattr [tag ^String s]
    (sedan.impl/decode-sequence oattr (sedan.impl/without-tag s) false))
  (encode* [^OrderedAttribute x buffer]
    (sedan.impl/encode-sequence [(.-attr x) (.-rank x)] buffer false))
  (compare* [a b]
    (sedan.impl/default-compare a b)))

