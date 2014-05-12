(defproject com.gfredericks/test.chuck "0.1.2-SNAPSHOT"
  :description "A dumping ground of test.check utilities"
  :url "https://github.com/fredericksgary/test.chuck"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]

  :plugins [[lein-release "1.0.5"]]
  :lein-release {:deploy-via :clojars})
