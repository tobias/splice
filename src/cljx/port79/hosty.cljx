(ns port79.hosty)

#+clj
(defn system-hash
  [x]
  (System/identityHashCode x))

#+cljs
(defn system-hash
  [x]
  (goog/getUid x))

#+clj
(defn current-time-ms
  []
  (System/currentTimeMillis))

#+cljs
(defn current-time-ms
  []
  (.now js/Date))

#+clj
(defn now
  []
  (java.util.Date.))

#+cljs
(defn now
  []
  (js/Date.))
