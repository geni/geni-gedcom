(ns gedcom-importer.web.service
  (:require [noir.core :refer [defpage defpartial]]
            [noir.server :refer [start]]
            [noir.request :as request]
            [hiccup.form :as form]
            [gedcom-importer.core :as importer]
            [geni.core :as geni]))

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
  (importer/import-gedcom (:tempfile gedcom) indi token)
  "DONE")

(defpage "/" []
  (form))

(defn -main []
  (start 8080))
