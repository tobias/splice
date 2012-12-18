(ns p79.crdt.space
  (:require [p79.crdt :as crdt]
            [p79.crdt.map :as map]
            [cemerick.utc-dates :refer (now)]
            [clojure.set :as set]
            [clojure.pprint :as pp])
  (:refer-clojure :exclude (read)))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(comment
;         P
;     ___/|\___
;  br/    |    \span
;         | 
;       strong

[ts tag 1 :tag :p]
[ts tag 1 :ref/html 2]
[ts tag 1 :ref/html 3]
[ts tag 1 :ref/html 4]
[ts tag 2 :tag :br]
[ts tag 2 :rank 0.0]
[ts tag 3 :tag :span]
[ts tag 3 :rank 1.0]
[ts tag 4 :tag :strong]
[ts tag 4 :rank 0.5]
)

(defmacro defroottype
  [type-name ctor-name type-tag value-name value-type value-pred]
  (let [value-sym (with-meta value-name {:tag value-type})
        value-field (symbol (str ".-" value-sym))
        type-tag (str "#" type-tag " ")
        [arg arg2] [(gensym) (gensym)]
        [type-arg type-arg2] (map #(with-meta % {:tag type-name}) [arg arg2])]
    `(do
       (deftype ~type-name [~value-sym]
         clojure.lang.IDeref
         (deref [~arg] (~value-field ~type-arg))
         Comparable
         (compareTo [~arg ~arg2]
           (compare (~value-field ~type-arg) (~value-field ~type-arg2)))
         Object
         (toString [~arg] (pr-str ~arg))
         (hashCode [~arg] (inc (hash (~value-field ~type-arg))))
         (equals [~arg ~arg2]
           (and (identical? (type ~arg) (type ~arg2))
             (= (~value-field ~type-arg) (~value-field ~type-arg2)))))
       (defmethod print-method ~type-name [~type-arg ^java.io.Writer w#]
         (.write w# ~type-tag)
         (print-method (~value-field ~type-arg) w#))
       (defmethod print-dup ~type-name [o# w#]
         (print-method o# w#))
       (#'pp/use-method pp/simple-dispatch ~type-name #'pp/pprint-simple-default)
       (defn ~(symbol (str ctor-name "?"))
         ~(str "Returns true iff the sole argument is a " type-name)
         [x#]
         (instance? ~type-name x#))
       (defn ~ctor-name
         ~(str "Creates a new " type-name)
         [e#]
         (when e#
           (cond
             (instance? ~type-name e#) e#
             (instance? ~value-type e#) (~(symbol (str type-name ".")) e#)
             :else (throw
                     (IllegalArgumentException.
                       (str "Cannot create " ~type-name " with value of type "
                         (class e#))))))))))

(defroottype Entity entity "entity" e String string?)
(defroottype Ref reference "ref" e Entity entity?)
(defroottype Tag tag "tag" t String string?)

(defprotocol ISpace
  (read [this]
     "Returns a lazy seq of the tuples in this space.")
  (write [this tuples] [this op-meta tuples]
     "Writes the given tuples to this space, optionally along with tuples derived from
a map of operation metadata.")
  (q [this query]
     "Queries this space, returning a seq of results per the query's specification")
  ;; TODO why doesn't this become part of queries, and a parameter to read?
  (as-of [this] [this time]
         "Returns a new space restricted to tuples written prior to [time]."))

;; TODO don't quite like the e/a/v naming here
;; s/p/o is used in RDF, but might be too much of a tie to semweb
;; [element datum state]? :-P

(defn metadata? [tuple] (not (:e tuple)))

(defprotocol AsTuples
  (as-tuples [x]))

(defrecord Tuple [time tag e a v]
  AsTuples
  (as-tuples [this] [this]))

(defn ->Tuple
  "Positional factory function for class p79.crdt.space.Tuple
that coerces any shortcut tag and entity values to Tag and Entiy instances.
This fn should therefore always be used in preference to the Tuple. ctor."
  [time op-tag e a v]
  (Tuple. time (tag op-tag) (entity e) a v))

(extend-protocol AsTuples
  nil
  (as-tuples [x] [])
  java.util.List
  (as-tuples [ls]
    (case (count ls)
      5 [(apply ->Tuple ls)]
      3 [(apply ->Tuple nil nil ls)]
      (throw (IllegalArgumentException.
               (str "Vector/list cannot be tuple-ized, bad size: " (count ls))))))
  java.util.Map
  (as-tuples [m]
    ;; if Tuple ever loses its inline impl of as-tuples, we *must*
    ;; rewrite this to dynamically extend AsTuples to concrete Map
    ;; types; otherwise, there's no way to prefer an extension to
    ;; Tuple over one to java.util.Map
    (if-let [e (:db/id m)]
      (let [time (:db/time m)
            tag (:db/tag m)
            s (seq (dissoc m :db/id))]
        (if s
          (for [[k v] s]
            (->Tuple time tag e k v))
          (throw (IllegalArgumentException. "Empty Map cannot be tuple-ized."))))
      (throw (IllegalArgumentException. "Map cannot be tuple-ized, no :db/id")))))

(defn- prep-tuples
  ([tuples]
    (prep-tuples (now) (uuid) tuples))
  ([time tag tuples]
    (let [tag (p79.crdt.space/tag tag)]
      (for [t tuples]
        (let [t (if (:time t) t (assoc t :time time))]
          (if (:tag t) t (assoc t :tag tag)))))))

(deftype MemSpace [tuples as-of metadata]
  ISpace
  (read [this]
    (if-not as-of
      (seq tuples)
      (filter #(< -1 (compare (:time %) as-of)) tuples)))
  (write [this tuples] (write this nil tuples))
  (write [this op-meta ts]
    (MemSpace. (->> (mapcat as-tuples ts)
                   (concat (as-tuples op-meta))
                   prep-tuples
                   (into tuples)) 
                 as-of
                 metadata))
  (as-of [this] as-of)
  (as-of [this time]
    (MemSpace. tuples time metadata))
  
  clojure.lang.IMeta
  (meta [this] metadata)
  clojure.lang.IObj
  (withMeta [this meta] (MemSpace. tuples as-of meta)))

(defn- time-comparator
  []
  (reify java.util.Comparator
    (compare [this x y]
      (compare (:time x) (:time y)))))

(defn in-memory
  []
  (MemSpace. #{} nil {}))


