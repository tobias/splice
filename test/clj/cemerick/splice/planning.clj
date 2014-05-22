; This Source Code Form is subject to the terms of the Mozilla Public License,
; v. 2.0. If a copy of the MPL was not distributed with this file, You can
; obtain one at http://mozilla.org/MPL/2.0/.

(ns cemerick.splice.planning
  (:require [cemerick.splice.memory.planning :as planning]
            [cemerick.splice.memory.query :as q])
  (:use clojure.test))

(deftest default-planner
  (is (= '[[?e :g ?v] (pos? ?v) [?e :a ?a] (pos? ?a) [?e :b 5 "tag"]]
        (#'planning/reorder-expression-clauses
          '[(pos? ?v)
            [?e :g ?v]
            (pos? ?a)
            [?e :a ?a]
            [?e :b 5 "tag"]]))))

(deftest predicate-expression-compilation
  (let [expr '(> 30 (inc ?x) ?y)
        fn (#'planning/expression-clause (q/clause-bindings expr) expr)]
    (is (= {:code ''(fn [{:syms [?x ?y]}] (> 30 (inc ?x) ?y))
            :clause `'~expr}
          (meta fn)))
    (is (= false ((eval fn) '{?x 29 ?y 20})))
    (is (= true ((eval fn) '{?x 28 ?y 20})))))
