(defproject com.gfredericks/test.chuck "0.1.22-SNAPSHOT"
  :description "A dumping ground of test.check utilities"
  :url "https://github.com/fredericksgary/test.chuck"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [instaparse "1.3.6"]]
  :deploy-repositories [["releases" :clojars]]
  :profiles {:dev {:dependencies
                   [[org.clojure/test.check "0.7.0"]
                    [org.clojure/clojurescript "0.0-2496"]]}}
  :plugins [[lein-cljsbuild "1.0.4-SNAPSHOT"]]
  :source-paths []
  :test-paths []
  :cljsbuild
  {:builds
   [{:id "node-dev"
     :source-paths ["test-cljs"
                    "src-cljs"
                    "src-target/cljs/node"]
     :notify-command ["node" "resources/run.js"]
     :compiler {:optimizations :none
                :static-fns true
                :target :nodejs
                :output-to "target/cljs/node_dev/tests.js"
                :output-dir "target/cljs/node_dev/out"
                :source-map true}}
    {:id "browser-dev"
     :source-paths ["test-cljs"
                    "src-cljs"
                    "src-target/cljs/browser"]
     :compiler {:optimizations :none
                :static-fns true
                :output-to "target/cljs/browser_dev/tests.js"
                :output-dir "target/cljs/browser_dev/out"
                :source-map true}}
    {:id "node-adv"
     :source-paths ["test-cljs"
                    "src-cljs"
                    "src-target/cljs/node"]
     :notify-command ["node" "target/cljs/node_adv/tests.js"]
     :compiler {:optimizations :advanced
                :target :nodejs
                :pretty-print false
                :output-to "target/cljs/node_adv/tests.js"
                :output-dir "target/cljs/node_adv/out"}}
    {:id "browser-adv"
     :source-paths ["test-cljs"
                    "src-cljs"
                    "src-target/cljs/browser"]
     :compiler {:optimizations :advanced
                :pretty-print false
                :output-to "target/cljs/browser_adv/tests.js"
                :output-dir "target/cljs/browser_adv/out"}}]}
  :aliases {"test-all"
            ^{:doc "Runs tests on multiple JVMs; profiles java-7
                    and java-8 should be defined outside this project."}
            ["do"
             "clean,"
             "with-profile" "+java-7" "test,"
             "clean,"
             "with-profile" "+java-8" "test"]})
