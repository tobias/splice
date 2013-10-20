(ns p79.math)

(def MAX #+clj Double/MAX_VALUE #+cljs (.-MAX_VALUE js/Number))
(def MIN #+clj Double/MIN_VALUE #+cljs (.-MIN_VALUE js/Number))
(def INF #+clj Double/POSITIVE_INFINITY #+cljs js/Infinity)
(def -INF (- INF))
