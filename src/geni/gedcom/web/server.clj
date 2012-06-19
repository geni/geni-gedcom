(ns geni.gedcom.web.server
  (:require [noir.server :refer [gen-handler start wrap-route add-middleware]]
            [ring.middleware.cors :refer [wrap-cors]]
            [geni.gedcom.web.common :refer [config]]
            [geni.gedcom.web.views api interface])
  (:gen-class))

(add-middleware wrap-cors
                :access-control-allow-origin #".*"
                :access-control-allow-headers "*"
                :access-control-allow-methods "*")

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