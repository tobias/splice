(ns cemerick.splice.hosty)

(defn system-hash
  [x]
  #+clj (System/identityHashCode x)
  #+cljs (goog/getUid x))

(defn current-time-ms
  []
  #+clj (System/currentTimeMillis)
  #+cljs (.now js/Date))

(defn now
  []
  #+clj (java.util.Date.)
  #+cljs (js/Date.))

