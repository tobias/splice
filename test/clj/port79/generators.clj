(ns port79.generators
  (:require [clojure.test.generative.generators :as gens]
            [clojure.test.generative :as gen]
            [clojure.stacktrace :refer (print-cause-trace)])
  (:use clojure.test)
  (:refer-clojure :exclude (char)))

(defn- bind-generative-test-period
  [f]
  (with-redefs [gen/*msec* 5000] (f)))

(use-fixtures :once bind-generative-test-period)

(defn sane-double
  "Generate a double, no NaN or infinities."
  ^double []
  (let [v (.nextDouble gens/*rnd*)]
    (if (or (Double/isNaN v) (Double/isInfinite v))
      (recur)
      v)))

(defn rand-fn
  "Returns a function that, when called, returns a random value from
   the provided options using test.generative's `rand-nth`."
  [& options]
  (fn [& args] (gens/rand-nth options)))

(defn number [] (gens/one-of sane-double gens/long))

(defn char
  "Returns a random character in the range (0-65536), excluding
   surrogate code points (which, taken in isolation, are not valid characters)."
  []
  (let [point (clojure.core/char (gens/uniform 1 65536))]
    (if (or (Character/isLowSurrogate point) (Character/isHighSurrogate point))
      (recur)
      point)))

(defn query-safe-symbol
  [& args]
  (loop [s (apply gens/symbol args)]
    (let [ns (namespace s)
          name (name s)]
      (if (or (= "_" name)
              (.startsWith name "?")
              (and ns (.startsWith ns "?")))
        (recur (apply gens/symbol args))
        s))))

(defn namespaced-keyword
  [sizer]
  (keyword (name (query-safe-symbol sizer)) (name (query-safe-symbol sizer))))

(defn namespaced-symbol
  [sizer]
  (symbol (name (query-safe-symbol sizer)) (name (query-safe-symbol sizer))))

(def scalars {:stress {number 0.25
                       gens/boolean 0.05
                       (partial gens/string char 20) 0.75
                       (rand-fn #(namespaced-keyword 20) #(gens/keyword 20)) 0.5
                       (rand-fn #(namespaced-symbol 20) #(query-safe-symbol 20)) 0.1}
              :readable {number 0.25
                         gens/boolean 0.05
                         #(gens/string gens/printable-ascii-char 10) 0.75
                         (rand-fn #(namespaced-keyword 1) #(gens/keyword 1)) 0.5
                         (rand-fn #(namespaced-symbol 1) #(query-safe-symbol 1)) 0.1}})

(defn ^:dynamic scalar
  ([] (scalar :readable))
  ([type] (gens/weighted (scalars type))))

(defn value
  ([] (value :readable))
  ([scalar-type] (value scalar-type nil))
  ([scalar-type additional-generators]
    (let [scalar-fn #(scalar scalar-type)]
      (gens/weighted (merge (scalars scalar-type)
                            {(constantly nil) 0.05
                             
                             ;; TODO are these composite values appropriate anymore here?
                             (rand-fn hash-set vector) 0.05 
                             #(gens/vec scalar-fn 4) 0.1
                             ; set with one member == that member, so
                             ; ensure we generate sets containing at least
                             ; two members
                             #(let [s (gens/set scalar-fn 4)]
                                (if (== 1 (count s))
                                  (recur)
                                  s)) 0.1}
                            additional-generators)))))

(defn entity*
  ([] (entity* 8))
  ([size] (entity* :readable size))
  ([scalar-type size]
    (entity* #(scalar scalar-type)
             #(value scalar-type)
             size))
  ([scalar-fn value-fn size]
    (gens/hash-map scalar-fn value-fn size)))

(defn nested-entity*
  ([] (nested-entity* :readable 4 4))
  ([scalar-type depth size]
    (entity* #(scalar scalar-type)
             (if (zero? depth)
               #(value scalar-type)
               #(value scalar-type (partial nested-entity* scalar-type (dec depth) size)))
             size)))