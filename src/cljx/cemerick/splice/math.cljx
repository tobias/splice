; This Source Code Form is subject to the terms of the Mozilla Public License,
; v. 2.0. If a copy of the MPL was not distributed with this file, You can
; obtain one at http://mozilla.org/MPL/2.0/.

(ns cemerick.splice.math)

(def MAX #+clj Double/MAX_VALUE #+cljs (.-MAX_VALUE js/Number))
(def MIN #+clj Double/MIN_VALUE #+cljs (.-MIN_VALUE js/Number))
(def INF #+clj Double/POSITIVE_INFINITY #+cljs js/Infinity)
(def -INF (- INF))
