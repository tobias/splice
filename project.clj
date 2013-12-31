(defproject com.cemerick/splice "0.0.1-SNAPSHOT"
  :description ""
  :url ""
  :license "proprietary"
  :resource-paths ["resources"]
  :source-paths ["src/cljx" "src/clj"] ;  "src/cljs"
  :test-paths ["test/cljx" "test/cljs" "test/clj" "target/test-classes"]
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.6.0-alpha3"]
                 [org.clojure/core.match "0.2.0"]
                 [org.clojure/core.incubator "0.1.1"]
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
              :builds [{:source-paths [ "target/classes" "target/test-classes"] ;"src/cljs"
                        :compiler {:output-to "target/testable.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]}
  
  :profiles {:dev {:dependencies [[com.cemerick/pomegranate "0.0.13"
                                   :exclusions [org.apache.httpcomponents/httpcore]]
                                  [org.clojure/clojurescript "0.0-2127"]]
                   :plugins [[lein-cljsbuild "1.0.0-alpha2"]
                             [com.cemerick/clojurescript.test "0.2.1"]
                             [com.cemerick/austin "0.1.3"]
                             [s3-wagon-private "1.1.2"]
                             [com.keminglabs/cljx "0.3.2"]]
                   :aliases {
                              "sanity-check" ["with-profile" "sanity-check"
                                              "do" "clean," "cljsc," "compile"]
                              "cleantest" ["do" "clean," "cljx" "once,"
                                           "test," "cljsbuild" "test"]}}
             :sanity-check {:aot :all}})
