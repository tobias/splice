(ns p79.crdt.space.root-type)

(defmacro defroottype
  [lang type-name ctor-name type-tag value-name value-pred]
  (let [value-field (symbol (str ".-" value-name))
        type-tag (str "#" type-tag " ")
        [arg arg2] [(gensym) (gensym)]
        [type-arg type-arg2] (map #(with-meta % {:tag type-name}) [arg arg2])]
    (if (= "clj" (name lang))
      `(do
         (deftype ~type-name [~value-name]
           clojure.lang.IDeref
           (deref [~arg] (~value-field ~type-arg))
           Comparable
           (compareTo [~arg ~arg2]
             (compare (~value-field ~type-arg) (~value-field ~type-arg2)))
           Object
           (toString [~arg] (pr-str ~arg))
           (hashCode [~arg] (inc (hash (~value-field ~type-arg))))
           (equals [~arg ~arg2]
             (and (instance? ~type-name ~arg2)
               (= (~value-field ~type-arg) (~value-field ~type-arg2))))
           ;; this here only for the benefit of Tombstone
           clojure.lang.ILookup
           (valAt [this# k#] (get ~value-name k#))
           (valAt [this# k# default#] (get ~value-name k# default#)))
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
               (~value-pred e#) (~(symbol (str type-name ".")) e#)
               :else (throw
                       (IllegalArgumentException.
                         (str "Cannot create " ~type-name " with value of type "
                           (type e#))))))))
      `(do
         (deftype ~type-name [~value-name]
           ~'IDeref
           (~'-deref [~arg] (~value-field ~type-arg))
           ~'IComparable
           (~'-compare [~arg ~arg2]
             (compare (~value-field ~type-arg) (~value-field ~type-arg2)))
           ~'IPrintWithWriter
           (~'-pr-writer [this# w# opts#]
             (~'-write w# "#entity ")
             (~'-pr-writer ~value-name w# opts#))
           ~'IHash
           (~'-hash [~arg] (inc (hash (~value-field ~type-arg))))
           ~'IEquiv
           (~'-equiv [~arg ~arg2]
             (and (instance? ~type-name ~arg2)
               (= (~value-field ~type-arg) (~value-field ~type-arg2))))
           ;; this here only for the benefit of Tombstone
           ~'ILookup
           (~'-lookup [this# k#] (get ~value-name k#))
           (~'-lookup [this# k# default#] (get ~value-name k# default#)))
         
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
               (~value-pred e#) (~(symbol (str type-name ".")) e#)
               :else (throw
                       (js/Error.
                         (str "Cannot create " ~type-name " with value of type "
                           (type e#)))))))
         (cljs.reader/register-tag-parser! '~ctor-name ~ctor-name)))))