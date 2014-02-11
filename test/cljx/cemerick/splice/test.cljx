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

