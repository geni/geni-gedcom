(ns geni.gedcom.web.models.interface
  (:require [noir.request :refer [ring-request]]
            [clj-http.client :as http]
            [geni.gedcom.web.common :refer [config]]
            [cheshire.core :refer [parse-string]]))

(defn ^:private create-url
  "Generate a new URL pathed to a location in the same place that
   the original request came from."
  [to]
  (let [request (ring-request)]
    (str (name (:scheme request)) "://"
         (:server-name request)
         (when-let [port (:server-port request)]
           (str ":" port))
         (config "base.url")
         to)))

(defn request-upload [file token]
  (-> (create-url "/upload")
      (http/post {:multipart {:gedcom file, :token token}})
      :body
      parse-string))

(defn request-import [id token]
  (-> (create-url "/import")
      (http/post {:form-params {:id id, :token token}})
      :body))