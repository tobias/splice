; This Source Code Form is subject to the terms of the Mozilla Public License,
; v. 2.0. If a copy of the MPL was not distributed with this file, You can
; obtain one at http://mozilla.org/MPL/2.0/.

(ns cemerick.splice.test)

(defn set-check
  "Returns a set containing the elements in [coll], throwing an exception if
[coll] contains any duplicate values."
  [coll]
  (let [set (set coll)]
    (when-not (== (count coll) (count set))
      (throw (ex-info "duplicate values found in set" {:dupes (for [[v cnt] (frequencies coll)
                                                                    :when (> cnt 1)]
                                                                v)})))
    set))

