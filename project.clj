(defproject org.quilt/splice "0.0.2-SNAPSHOT"
  :description ""
  :url "http://github.com/QuiltProject/splice"
  :license {:name "Mozilla Public License v2.0+"
            :url "https://www.mozilla.org/MPL/"}
  :source-paths ["src/cljx" "src/clj"] ;  "src/cljs"
  :test-paths ["test/cljx" "test/clj" "target/test-classes"] ; "test/cljs" 
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2227"]
                 [org.clojure/core.match "0.2.0"]
                 [org.clojure/core.incubator "0.1.1"]
                 [org.quilt/sedan "0.0.5"]
                 [org.clojure/math.combinatorics "0.0.4"]
                 [com.cemerick/cljs-sanity "0.0.1"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]]
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
  
  :cljsbuild {:test-commands {"node" ["node" :node-runner "target/testable.js"]}
              :builds [{:source-paths ["target/classes" "target/test-classes"] ;"src/cljs"
                        :compiler {:output-to "target/testable.js"
                                   :optimizations :advanced
                                   ; :source-map "target/testable.js.map"
                                   :pretty-print true}}]}

  ; coping with https://github.com/emezeske/lein-cljsbuild/issues/303
  ; offending bits are the replication tests (is assertions getting caught up in `go` transforms)
  :jvm-opts []

  :profiles {:dev {:dependencies [[com.cemerick/double-check "0.5.7"]]
                   :plugins [[lein-cljsbuild "1.0.3"]
                             [com.cemerick/clojurescript.test "0.3.1"]
                             [com.keminglabs/cljx "0.3.3-SNAPSHOT"]
                             [com.cemerick/austin "0.1.5-SNAPSHOT"]
                             ]
                   :aliases {"sanity-check" ["with-profile" "sanity-check"
                                              "do" "clean," "cljsc," "compile"]
                              "quicktest" ["do" "clean," "cljx" "once,"
                                           "test," "cljsbuild" "test"]
                              "cljs-quicktest" ["do", "clean," "cljx" "once,"
                                                "cljsbuild" "test"]
                              "cleantest" ["with-profile" "rigorous" "quicktest"]
                              "release" ["do" ["clean"] ["cljx" "once"] ["deploy"] ["deploy" "clojars"]]}}
             :sanity-check {:aot :all}
             ; pushes the number of trials run in tests up to some (conservative) absurd level
             :rigorous [:default
                        {:jvm-opts ["-Dquickcheck-times=10000000"]
                         :cljsbuild {:test-commands {"node"
                                                     ^:replace ["node" :runner "this.quickcheck_times='10000000'" "target/testable.js"]}}}]}

  :deploy-repositories {"releases" {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
                                    :creds :gpg}
                        "snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/"
                                     :creds :gpg}}

  ;;maven central requirements
  :scm {:url "https://github.com/QuiltProject/splice.git"}
  :pom-addition [:developers [:developer
                              [:name "Chas Emerick"]
                              [:url "http://cemerick.com"]
                              [:email "chas@cemerick.com"]
                              [:timezone "-5"]]])

