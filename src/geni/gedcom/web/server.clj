(ns geni.gedcom.web.server
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.core :refer [defroutes GET]]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.util.response :refer [resource-response]]
            [geni.gedcom.web.common :refer [config]]
            [geni.gedcom.web.views.api :refer [api-routes]]
            [geni.gedcom.web.views.system :refer [system-routes]])
  (:gen-class))

(def api
  (-> api-routes
      handler/api
      wrap-multipart-params
      (wrap-cors
       :access-control-allow-origin #".*"
       :access-control-allow-headers "X-Requested-With, X-File-Name, Origin, Content-Type"
       :access-control-allow-methods "GET, POST")))

(defroutes handler
  (GET "/" [] (resource-response "public/index.html"))
  api
  system-routes
  (route/resources "/")
  (route/not-found "Page not found."))

;; war test code
(comment
  (import org.mortbay.jetty.Server
          org.mortbay.jetty.webapp.WebAppContext)
  
  (doto (Server. 8080)
    (.setHandler (WebAppContext. "target/importer.war" "/"))
    .start))