(ns port79.hosty)

(defn ^:clj system-hash
  [x]
  (System/identityHashCode x))

(defn ^:cljs system-hash
  [x]
  (goog/getUid x))

(defn ^:clj current-time-ms
  []
  (System/currentTimeMillis))

(defn ^:cljs current-time-ms
  []
  (.now js/Date))

(defn ^:clj now
  []
  (java.util.Date.))

(defn ^:cljs now
  []
  (js/Date.))