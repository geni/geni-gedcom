(ns geni.gedcom.web
  (:require [noir.core :refer [defpage defpartial]]
            [noir.server :refer [start gen-handler]]
            [hiccup.form :refer [label text-area file-upload submit-button]]
            [hiccup.util :refer [*base-url*]]
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
   [:form {:method "POST" :action (str *base-url* "/import") :enctype "multipart/form-data"}
     (label "token-label" "Geni API Token: ")
     (text-area "token")
     [:br]
     (label "record-label" "Your INDI Record: ")
     (text-area "indi")
     [:br]
     (label "gedcom-label" "GEDCOM File: ")
     (file-upload "gedcom")
     [:br]
     (submit-button "Submit")]])

(defpage [:post "/import"] {:keys [gedcom indi token]}
  (binding [geni/*base* (config "url")
            geni/*insecure* (Boolean/valueOf (config "insecure"))]
    (import-gedcom (:tempfile gedcom) indi token))
  "DONE")

(defpage "/" []
  (form))

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