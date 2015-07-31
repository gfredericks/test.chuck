(defproject com.gfredericks/test.chuck "0.1.21"
  :description "A dumping ground of test.check utilities"
  :url "https://github.com/fredericksgary/test.chuck"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [instaparse "1.3.6"]]
  :deploy-repositories [["releases" :clojars]]
  :profiles {:dev {:dependencies
                   [[org.clojure/test.check "0.7.0"]]}}
  :aliases {"test-all"
            ^{:doc "Runs tests on multiple JVMs; profiles java-7
                    and java-8 should be defined outside this project."}
            ["do"
             "clean,"
             "with-profile" "+java-7" "test,"
             "clean,"
             "with-profile" "+java-8" "test"]})
