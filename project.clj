(defproject com.cemerick/splice "0.0.1-SNAPSHOT"
  :description ""
  :url ""
  :license "proprietary"
  :source-paths ["src/cljx" "src/clj"] ;  "src/cljs"
  :test-paths ["test/cljx" "test/clj" "target/test-classes"] ; "test/cljs" 
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.6.0-alpha3"]
                 [org.clojure/clojurescript "0.0-2138"]
                 [org.clojure/core.match "0.2.0"]
                 [org.clojure/core.incubator "0.1.1"]
                 [com.cemerick/sedan "0.0.3"]
                 [org.clojure/math.combinatorics "0.0.4"]
                 [com.cemerick/cljs-sanity "0.0.1"]]
  :test-selectors {:default (complement :integration)
                   :integration :integration
                   :all (constantly true)}
  
  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}
                  
                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :clj}
                  
                  {:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :cljs}
                  
                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :cljs}]}
  
  :cljsbuild {:test-commands {"phantom" ["phantomjs" :runner "target/testable.js"]}
              :builds [{:source-paths ["target/classes" "target/test-classes"] ;"src/cljs"
                        :compiler {:output-to "target/testable.js"
                                   :optimizations :advanced
                                   ; :source-map "target/testable.js.map"
                                   :libs [""]
                                   :pretty-print true}}]}
  
  :profiles {:dev {:dependencies [[com.cemerick/double-check "0.5.4-SNAPSHOT"]]
                   :plugins [[lein-cljsbuild "1.0.0-alpha2"]
                             [com.cemerick/clojurescript.test "0.2.1"]
                             [com.keminglabs/cljx "0.3.2"]
                             [com.cemerick/austin "0.1.4-SNAPSHOT"]
                             [s3-wagon-private "1.1.2"]
                             ]
                   :aliases {"sanity-check" ["with-profile" "sanity-check"
                                              "do" "clean," "cljsc," "compile"]
                              "quicktest" ["do" "clean," "cljx" "once,"
                                           "test," "cljsbuild" "test"]
                              "cleantest" ["with-profile" "rigorous" "quicktest"]}}
             :sanity-check {:aot :all}
             ; pushes the number of trials run in tests up to some (conservative) absurd level
             :rigorous [:default
                        {:jvm-opts ["-Dquickcheck-times=10000000"]
                         :cljsbuild {:test-commands {"phantom"
                                                     ^:replace ["phantomjs" :runner "window.quickcheck_times='10000000'" "target/testable.js"]}}}]})
