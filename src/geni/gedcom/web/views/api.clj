(ns geni.gedcom.web.views.api
  (:require [geni.gedcom.web.models.api :refer [future-clean import-records count-profiles cache names cache-records]]
            [geni.gedcom.web.common :refer [catch-exception-string]]
            [gedcom.core :refer [parse-gedcom]]
            [geni.gedcom.common :refer [to-geni]]
            [noir.core :refer [defpage]]
            [noir.response :refer [json]]))

(defpage [:post "/upload"] {:keys [gedcom token]}
  (json
   (let [results (catch-exception-string
                  (parse-gedcom (:tempfile gedcom) to-geni))]
     (if (map? results)
       (do (future-clean token (:id (swap! cache cache-records results token)))
           {:names (names results)})
       {:error results}))))

(defpage [:post "/import"] {:keys [id token]}
  (json
   (if-let [records (:records (get @cache token))]
     (let [results (import-records records id token)]
       (swap! cache dissoc token)
       (if (map? results)
         {:profiles_imported (count-profiles results)}
         {:error results}))
     {:error "No records associated with this token."})))