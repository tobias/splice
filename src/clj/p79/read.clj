(ns p79.read
  (:require [clojure.java.io :as io]))

(defn read-seq
  [source]
  (letfn [(value-seq [stream]
            (lazy-seq
              (binding [*read-eval* false]
                (if-let [x (read stream false nil)]
                  (cons x (value-seq stream))
                  (.close stream)))))]
    (value-seq (java.io.PushbackReader. (io/reader source)))))