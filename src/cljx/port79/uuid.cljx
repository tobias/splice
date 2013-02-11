(ns port79.uuid
  (:require [port79.hosty :refer (now current-time-ms)]))

(defn ^:clj uuid? [x] (instance? java.util.UUID x))
(defn ^:cljs uuid? [x] (instance? cljs.core.UUID x))

;; would go look at the default readers to get the fn that's used to 
;; read uuid string literals, but that's `default-data-readers` in Clojure
;; (keyed by symbols) and `*tag-table*` in ClojureScript (keyed by strings)!!!
(defn ^:clj uuid
  [string]
  (if (uuid? string)
    string
    (java.util.UUID/fromString string)))

(defn ^:cljs uuid
  [string]
  (if (uuid? string)
    string
    (cljs.core.UUID. string)))

(def ^:clj ^:private rng (java.security.SecureRandom.))
(defn ^:clj time-uuid*
  "Returns a sequential UUID. Guaranteed to:

(a) monotonically increase when printed lexicographically
(b) contain [time] (or the current time in millis) as the most significant bits"
  ([] (time-uuid* (current-time-ms)))
  ([time] (java.util.UUID. time (.nextLong rng))))

(defn- ^:cljs hex-pad
  [n len]
  (let [s (.toString n 16)
        diff (- len (count s))
        pad (when (pos? diff) (apply str (repeat diff "0")))]
    (str pad s)))

(defn- ^:cljs bits->string
  [high low]
  (let [high (re-seq #"(.{8})(.{4})(.{4})" (hex-pad high 16))
        low (re-seq #"(.{4})(.{12})" (hex-pad low 16))]
    (->> (rest (first low))
      (concat (rest (first high)))
      (interpose "-")
      (apply str))))

(def ^:cljs max-long 9223372036854775807)
(defn ^:cljs time-uuid*
  ([] (time-uuid* (current-time-ms)))
  ([time] (uuid (bits->string time (rand-int max-long)))))

(def time-uuid (comp str time-uuid*))

(defn ^:clj random-uuid* [] (java.util.UUID/randomUUID))
(defn ^:cljs random-uuid* []
  (uuid (bits->string (rand-int max-long) (rand-int max-long))))

(defn random-uuid [] (str (random-uuid*)))