

(db/save-view db
              "queries"
              (db/view-server-fns :cljs
                {:entity-attrs {:map #(js/emit
                                        (aget % "e")
                                        (aget % "a"))}}))

5

(db/save-view db
              "queries"
              (db/view-server-fns {:language :cljs
                                   :main 'lookup/main}
                {:entity-lookup
                 {:map [(ns lookup)
                        (defn ^:export main [%]
                          (js/emit
                            (js/Array (aget % "e")
                                      (aget % "a"))
                            (aget % "v")))]
      :reduce [(ns lookup)
               (defn map->js [m]
                 (let [out (js-obj)]
                   (doseq [[k v] m]
                     (aset out (name k) v))
                   out))
               
               (defn clj->js
                 [x]
                 (cond
                   (string? x) x
                   (keyword? x) (name x)
                   (map? x) (.-strobj (reduce (fn [m [k v]]
                                                (assoc m (clj->js k) (clj->js v))) {} x))
                   (coll? x) (apply array (map clj->js x))
                   :else x))
               
               (defn ^:export main
                 [keys values rereduce]
                 (->> values
                   (interleave (map (comp second first) keys))
                   (partition 2)
                   (reduce
                     (fn [m [k v]]
                       (update-in m [k] (fnil conj #{}) v))
                     {})
                   clj->js))]}}))