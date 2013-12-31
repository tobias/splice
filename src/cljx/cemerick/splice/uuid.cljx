(ns cemerick.splice.uuid
  (:require [cemerick.splice.hosty :refer (now current-time-ms)]))

#+clj
(defn uuid? [x] (instance? java.util.UUID x))
#+cljs
(defn uuid? [x] (instance? cljs.core.UUID x))

;; would go look at the default readers to get the fn that's used to 
;; read uuid string literals, but that's `default-data-readers` in Clojure
;; (keyed by symbols) and `*tag-table*` in ClojureScript (keyed by strings)!!!
#+clj
(defn uuid
  [string]
  (if (uuid? string)
    string
    (java.util.UUID/fromString string)))

#+cljs
(defn uuid
  [string]
  (if (uuid? string)
    string
    (cljs.core.UUID. string)))

#+clj
(def ^:private rng (java.security.SecureRandom.))
#+clj
(defn time-uuid*
  "Returns a sequential UUID. Guaranteed to:

(a) monotonically increase when printed lexicographically
(b) contain [time] (or the current time in millis) as the most significant bits"
  ([] (time-uuid* (current-time-ms)))
  ([time] (java.util.UUID. time (.nextLong rng))))

#+cljs
(defn- hex-pad
  [n len]
  (let [s (.toString n 16)
        diff (- len (count s))
        pad (when (pos? diff) (apply str (repeat diff "0")))]
    (str pad s)))

#+cljs
(defn- bits->string
  [high low]
  (let [high (re-seq #"(.{8})(.{4})(.{4})" (hex-pad high 16))
        low (re-seq #"(.{4})(.{12})" (hex-pad low 16))]
    (->> (rest (first low))
      (concat (rest (first high)))
      (interpose "-")
      (apply str))))

#+cljs
(def max-long 9223372036854775807)
#+cljs
(defn time-uuid*
  ([] (time-uuid* (current-time-ms)))
  ([time] (uuid (bits->string time (rand-int max-long)))))

(defn uuid-str
  [uuid]
  #+clj (str uuid)
  #+cljs (.-uuid uuid))

(def time-uuid (comp uuid-str time-uuid*))

#+clj
(defn random-uuid* [] (java.util.UUID/randomUUID))
#+cljs
(defn random-uuid* []
  (uuid (bits->string (rand-int max-long) (rand-int max-long))))

(def random-uuid (comp uuid-str random-uuid*))
