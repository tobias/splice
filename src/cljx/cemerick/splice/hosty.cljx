; This Source Code Form is subject to the terms of the Mozilla Public License,
; v. 2.0. If a copy of the MPL was not distributed with this file, You can
; obtain one at http://mozilla.org/MPL/2.0/.

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

