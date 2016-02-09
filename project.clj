(defproject com.gfredericks/test.chuck "0.2.7-SNAPSHOT"
  :description "A dumping ground of test.check utilities"
  :url "https://github.com/fredericksgary/test.chuck"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0" :scope "provided"]
                 [org.clojure/clojurescript "1.7.48" :scope "provided"]
                 [org.clojure/test.check "0.9.0"]
                 [clj-time "0.10.0"]
                 [com.andrewmcveigh/cljs-time "0.3.11"]
                 [instaparse "1.3.6"]]
  :deploy-repositories [["releases" :clojars]]
  :profiles {:circle-ci {:jvm-opts ["-Xmx1g" "-server"]}}
  :plugins [[lein-cljsbuild "1.0.6"]
            [lein-doo "0.1.4"]]

  :cljsbuild
  {:builds
   [{:id "node-test"
     :source-paths ["src" "test"]
     :compiler {:output-to "target/tests.js"
                :output-dir "target/node"
                :main 'com.gfredericks.test.chuck.runner
                :optimizations :none
                :hashbang false
                :target :nodejs}}
    {:id "test"
     :source-paths ["src" "test"]
     :compiler {:output-to "target/tests.js"
                :main 'com.gfredericks.test.chuck.runner
                :optimizations :none}}]}

  :aliases {"test-all"
            ^{:doc "Runs tests on multiple JVMs; profiles java-7
                    and java-8 should be defined outside this project."}
            ["do"
             "clean,"
             "with-profile" "+java-7" "test,"
             "clean,"
             "with-profile" "+java-8" "test"]})
