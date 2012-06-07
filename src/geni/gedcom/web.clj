(ns geni.gedcom.web
  (:require [noir.core :refer [defpage defpartial]]
            [noir.server :refer [start gen-handler]]
            [noir.request :as request]
            [hiccup.form :as form]
            [geni.gedcom.import :refer [import-gedcom]]
            [geni.core :as geni]
            [clojure.java.io :refer [resource reader]])
  (:import java.util.Properties)
  (:gen-class))

(defn read-properties []
  (with-open [f (reader (resource "gedcom.properties"))]
    (into {} (doto (Properties.) (.load f)))))

(def config (read-properties))

(defpartial form []
  [:body
   [:form {:method "POST" :action "/import" :enctype "multipart/form-data"}
     (form/label "token-label" "Geni API Token: ")
     (form/text-area "token")
     [:br]
     (form/label "record-label" "Your INDI Record: ")
     (form/text-area "indi")
     [:br]
     (form/label "gedcom-label" "GEDCOM File: ")
     (form/file-upload "gedcom")
     [:br]
     (form/submit-button "Submit")]])

(defpage [:post "/import"] {:keys [gedcom indi token]}
  (prn token)
  (binding [geni/*base* (config "url")
            geni/*insecure* (Boolean/valueOf (config "insecure"))]
    (import-gedcom (:tempfile gedcom) indi token))
  "DONE")

(defpage "/" []
  (form))

(def handler (gen-handler {:mode :prod}))

(defn -main []
  (start 8080))

;; war test code
(comment
  (import org.mortbay.jetty.Server
          org.mortbay.jetty.webapp.WebAppContext)
  
  (doto (Server. 8080)
    (.setHandler (WebAppContext. "target/importer.war" "/"))
    .start))