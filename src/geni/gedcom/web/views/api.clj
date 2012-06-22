(ns geni.gedcom.web.views.api
  (:require [geni.gedcom.web.models.api :refer [future-clean import-records count-profiles cache names cache-records]]
            [geni.gedcom.web.common :refer [catch-exception-string]]
            [gedcom.core :refer [parse-gedcom]]
            [geni.gedcom.common :refer [to-geni]]
            [compojure.core :refer [defroutes POST]]
            [ring.util.response :refer [response status content-type]]
            [cheshire.core :refer [generate-string]]))

(defn respond [processed]
  (-> (generate-string processed)
      response
      (status (if (:error processed)
                422
                200))
      (content-type "application/json")))

(defn upload-ged [gedcom token]
  (respond
   (let [results (catch-exception-string
                  (parse-gedcom (:tempfile gedcom) to-geni))]
     (if (map? results)
       (do (future-clean token (:id (swap! cache cache-records results token)))
           {:names (names results)})
       {:error results}))))

(defn import-ged [id token]
  (respond
   (if-let [records (:records (get @cache token))]
     (let [results (import-records records id token)]
       (swap! cache dissoc token)
       (if (map? results)
         {:profiles_imported (count-profiles results)}
         {:error results}))
     {:error "No records associated with this token."})))

(defroutes api-routes
  (POST "/upload" {{:keys [gedcom token]} :params}
    (upload-ged gedcom token))
  (POST "/import" {{:keys [id token]} :params}
    (import-ged id token)))