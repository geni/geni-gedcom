(ns geni.gedcom.web.views.api
  (:require [geni.gedcom.web.models.api :refer [prepare-gedcom import-gedcom check-progress]]
            [compojure.core :refer [defroutes POST GET]] 
            [noir.response :refer [json status]]
            [clojure.tools.logging :refer [info]]))

(defn respond [processed]
  (->> (json processed)
       (status (if (:error processed)
                 422
                 200))))

(defn upload-ged [gedcom token]
  (info (str "Starting upload for " token))
  (respond (prepare-gedcom gedcom token)))

(defn import-ged [id token]
  (info (str "Importing records for " token))
  (respond (import-gedcom id token)))

(defn progress [token]
  (respond (check-progress token)))

(defroutes api-routes
  (POST "/upload" {{:keys [gedcom token]} :params}
    (upload-ged gedcom token))
  (POST "/import" {{:keys [id token]} :params}
    (import-ged id token))
  (GET "/progress" {{:keys [token]} :params}
    (progress token)))