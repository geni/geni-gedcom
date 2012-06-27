(defproject geni-gedcom "0.0.10"
  :description "A GEDCOM to Geni importer."
  :url "http://github.com/geni/geni-gedcom"
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [geni-clj-sdk "0.1.4-alpha4"]
                 [gedcom "0.1.0-alpha8"]
                 [useful "0.8.3-alpha2"]
                 [compojure "1.1.0"]
                 [lib-noir "0.1.1"]
                 [flatland/ring-cors "0.0.5"]]
  :plugins [[lein-ring "0.7.1"]]
  :ring {:handler geni.gedcom.web.server/handler
         :war-exclusions [#"gedcom.properties"]}
  :main geni.gedcom.web.server)

(use '[robert.hooke :only [add-hook]]
     '[clojure.java.shell :only [sh]])

(require '[leiningen.compile :as c])

(defn info-hook [f project & args]
  (spit "resources/leinversion" (:version project))
  (spit "resources/gitsha" (:out (sh "git" "rev-parse" "HEAD"))))

(add-hook #'c/compile info-hook)