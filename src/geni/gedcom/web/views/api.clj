(ns geni.gedcom.web.views.api
  (:require [geni.gedcom.web.models.api :refer [future-clean import-records count-profiles cache names cache-records]]
            [geni.gedcom.web.common :refer [catch-exception-string]]
            [gedcom.core :refer [parse-gedcom]]
            [geni.gedcom.common :refer [to-geni]]
            [compojure.core :refer [defroutes POST]] 
            [noir.response :refer [json status]]
            [clojure.tools.logging :refer [info error]]))

(defn respond [processed]
  (->> (json processed)
       (status (if (:error processed)
                 422
                 200))))

(defn upload-ged [gedcom token]
  (info (str "Starting upload for " token))
  (respond
   (let [results (catch-exception-string (str "Error parsing gedcom for " token)
                   (parse-gedcom (:tempfile gedcom) to-geni))]
     (if (map? results)
       (do (future-clean token (:id (swap! cache cache-records results token)))
           (info (str "Successfully parsed gedcom records for " token))
           {:names (names results)})
       {:error results}))))

(defn import-ged [id token]
  (info (str "Importing records for " token))
  (respond
   (if-let [records (:records (get @cache token))]
     (let [results (import-records records id token)]
       (swap! cache dissoc token)
       (if (map? results)
         (let [count (count-profiles results)]
           (info (str "Successfully imported " count " profiles for " token))
           {:profiles_imported count})
         {:error results}))
     (let [message (str "No records associated with token " token)]
       (error message)
       {:error message}))))

(defroutes api-routes
  (POST "/upload" {{:keys [gedcom token]} :params}
    (upload-ged gedcom token))
  (POST "/import" {{:keys [id token]} :params}
    (import-ged id token)))