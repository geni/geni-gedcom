(defproject geni-gedcom "0.1.0-SNAPSHOT"
  :description "A GEDCOM to Geni importer."
  :url "http://github.com/geni/gedcom-importer"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [geni-clj-sdk "0.1.4-alpha4"]
                 [gedcom "0.1.0-alpha6"]
                 [useful "0.8.3-alpha2"]
                 [noir "1.3.0-beta2"]]
  :plugins [[lein-ring "0.7.1"]]
  :ring {:handler geni.gedcom.web/handler}
  :main geni.gedcom.web)
