(ns geni.gedcom.web.server
  (:require [noir.server :refer [gen-handler start]]
            [geni.gedcom.web.common :refer [config]]
            [geni.gedcom.web.views api interface])
  (:gen-class))

(def handler (gen-handler {:mode :prod
                           :base-url (config "base.url")}))

(defn -main []
  (start 8080 {:mode :prod
               :base-url (config "base.url")}))

;; war test code
(comment
  (import org.mortbay.jetty.Server
          org.mortbay.jetty.webapp.WebAppContext)
  
  (doto (Server. 8080)
    (.setHandler (WebAppContext. "target/importer.war" "/"))
    .start))