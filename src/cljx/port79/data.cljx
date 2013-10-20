(ns port79.data
  (:require #_[port79.hosty :as hosty]
            [clojure.walk :as walk]
            [clojure.data :as data]
            [clojure.zip :as z]
            [clojure.string :as str]))

;; I *really* thought that I would have to add some kind of non-associative
;; support for diffing sequential values (i.e. levenshtein, etc) and wedge it
;; into clojure.data/diff, but it turns out that, once in a set, entity
;; states diff *beautifully*.  Sequential values, translated into states,
;; turn into just another batch of opaque values within a set, and thus
;; are readily diffed by clojure.data/diff — not a sequential value to be found.
(defn diff* 
  "Returns a diff of two sequences of states, represented as
   [states-to-withdraw, new-states].

[multi-valued-attributes] should be a set of attributes which have
a cardinality >= 2."
  [multi-valued-attributes before after]
  (let [[bg ag] (map set [before after])
        [dels adds] (data/diff bg ag)
        upserted (->> adds
                   (remove (comp multi-valued-attributes second))
                   (map (partial take 2))
                   set)
        dels (->> dels
               (remove #(upserted (take 2 %)))
               set)]
    [dels adds]))

#_(defn diff->asserts
  [[dels adds]]
  (concat
    (map (partial cons :db/add) adds)
    (map (partial cons :db/retract) dels)))

#_(defn graph->datoms
  [g]
  (apply concat
    (for [[eid e] g
          [a v] e]
      (if (coll? v)
        (for [v v] [eid a v])
        [[eid a v]]))))

#_(defn entity->node
  ([e] (entity->node (:db/id e) (dissoc e :db/id)))
  ([id e] {id e}))

#_(defn entities->graph
  [es]
  (apply merge (map entity->node es)))

;; sample content graph
#_(def graph
  {1 {:type :topic :title "Blog post" :owner "cemerick"
              :html/content [2 3 4]}
   2 {:type :html/p :html/content ["I've always liked this Ben Franklin quote:"]}
   3 {:type :html/blockquote
              :html/class "callout"
              :html/content ["Those willing to trade liberty for security deserve neither."]}
   4 {:type :html/p :html/content ["And yet, so many continue to make that "
                                           5
                                           " tradeoff!"]}
   5 {:type :html/em :html/content ["exact"]}
   10 {:type :topic :title "My favorite quotes" :owner "Sarah"
               :html/content [11 12]}
   11 {:type :html/p :content ["A selection of my favorite quotes:"]}
   12 {:type :ref :refs [3]}})

(defprotocol Element
  (-tag [this])
  (-attrs [this])
  (-content [this]))

(defprotocol Text
  (-text [this]))

#+clj
(extend-protocol Element
  clojure.lang.IPersistentMap
  (-tag [this] (-> this :tag name))
  (-attrs [this] (:attrs this))
  (-content [this] (:content this)))

#+clj
(extend-protocol Text
  String
  (-text [this] this))

#+cljs
(extend-protocol Element
  js/Element
  (-tag [this] (.-tagName this))
  (-attrs [this] (into {} (.-attributes this)))
  (-content [this] (.-childNodes this)))

#+cljs
(extend-protocol Text
  js/Text
  (-text [this] (.-wholeText this))
  js/String
  (-text [this] this))

#+clj
(defn- attribute? [x] (instance? clojure.lang.IMapEntry x))
#+cljs
(defn- attribute? [x] (satisfies? IMapEntry x))

(defn tagname [x] (str/lower-case (-tag x)))

(defn html->hiccup
  [root]
  (if (satisfies? Text root)
    (-text root)
    (into
      [(-> root tagname keyword)
       (-attrs root)]
      (map html->hiccup (-content root)))))

(defn- integer
  [x]
  #+clj (Long/parseLong x)
  #+cljs (js/parseInt x))

#_
(defn html->entities
  [html]
  (let [tempid (hosty/tempid-factory)]
    (letfn [(html->entities*
              [idx html]
              (cond
                (satisfies? Element html)
                (let [attrs (-attrs html)
                      id (or (get attrs "data--eid") (tempid html))
                      attrs (for [[k v :as e] attrs
                                  :when (not (re-find #"data--eid.*" k))]
                              (with-meta e
                                (if-let [id (get attrs (str "data--eid-" k))]
                                  {:db/id (integer id)}
                                  {:db/id (tempid e)})))
                      content (-> html -content vec)]
                  (concat
                    [(merge
                       {:db/id id
                        :graph/tag (keyword "html" (tagname html))}
                       (when (seq attrs)
                         (->> attrs
                           (map (comp :db/id meta))
                           (into #{})
                           (hash-map :graph/attrs)))
                       (when idx
                         {:graph/rank (* idx 1000)})  ;; yay, BASIC!
                       (when (seq content)
                         {:graph/refs (into #{} (map tempid content))}))]
                    (mapcat (partial html->entities* nil) attrs)
                    (mapcat html->entities* (range) content)))
                
                (satisfies? Text html)
                [{:db/id (tempid html)
                  :graph/tag :graph/text
                  :graph/rank (* idx 1000)
                  :graph/str (-text html)}]
                
                (attribute? html)
                [{:db/id (-> html meta :db/id)
                  :graph/tag :graph/attr
                  :graph/attr (-> html key keyword)
                  :graph/str (-> html val str)}]
                
                :else
                (throw (IllegalArgumentException.
                         (str "unexpected type of value in html: " (type html))))))]
      (html->entities* nil html))))
#_#_
(defn graph-root-ids
  [g]
  (assert (map? g) "graph should be a map [ids -> nodes]")
  (apply disj
         (set (keys g))
         (->> (vals g)
           (mapcat (comp (partial apply concat)
                         (juxt :graph/attrs :graph/refs)))
           (remove nil?))))

(def html->graph (comp entities->graph html->entities))

; [port79.xml :as xml]
#_(def html->graph (comp xml->graph xml/hiccup->xml))

