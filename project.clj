(defproject com.gfredericks/test.chuck "0.2.11"
  :description "A dumping ground of test.check utilities"
  :url "https://github.com/fredericksgary/test.chuck"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.10.520" :scope "provided"]
                 [org.clojure/test.check "0.10.0-RC1"]
                 [clj-time "0.10.0"]
                 [com.andrewmcveigh/cljs-time "0.5.1"]
                 [instaparse "1.3.6"]]
  :deploy-repositories [["releases" :clojars]]
  :profiles {:circle-ci {:jvm-opts ["-Xmx1g" "-server"]}}
  :plugins [[lein-cljsbuild "1.0.6"]
            [lein-doo "0.1.4"]
            [com.gfredericks/lein-all-my-files-should-end-with-exactly-one-newline-character "0.1.0"]]

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
            ^{:doc "Runs tests on multiple JVMs; profiles java-8
                    and java-11 should be defined outside this project."}
            ["do"
             "clean,"
             "with-profile" "+java-8" "test,"
             "clean,"
             "with-profile" "+java-11" "test"]})
