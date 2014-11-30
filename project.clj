(defproject com.gfredericks/test.chuck "0.1.6-SNAPSHOT"
  :description "A dumping ground of test.check utilities"
  :url "https://github.com/fredericksgary/test.chuck"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :profiles {:dev {:dependencies
                   [[org.clojure/test.check "0.5.7"]
                    [instaparse "1.3.4"]]}})
