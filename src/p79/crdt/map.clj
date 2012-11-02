(ns p79.crdt.map
  (:refer-clojure :exclude (remove)))

(defprotocol Map
  (add [this k v] [this k v tags])
  (remove [this k] [this k tags])
  (lookup [this k]))

(comment
  M = #{K #_> #{V V' V'' …}}
  
  ;Sequential spec:
  {} (add K V) {K #_> #{V}}
  {K #_> #{V}} (add K V') {K #_> #{V V'}}
  {K #_> #{V}} (remove K) {}

  
  * (not linearizable?)
  
  {… …} (add K V) || (remove K) {?? ??}
  
  * error state?
  * LWW?
  * add wins?
  * remove wins?
  
  {… …} (add K V) || (add K V') {?? ??}
  
  (how to choose between concurrent adds with same key)
  * error state?
  * LWW?
  * multimap
  ** All values are MV-registers
  ** map effectively a set of key+tag, each with a payload V
  * V-specific merge?
    
  )