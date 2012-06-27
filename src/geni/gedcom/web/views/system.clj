(ns geni.gedcom.web.views.system
  (:require [compojure.core :refer [defroutes GET]]
            [clojure.java.io :refer [resource]]
            [noir.response :refer [json]]))

(defn status []
  (let [sha (when-let [sha (resource "gitsha")] (slurp sha))
        version (when-let [version (resource "leinversion")] (slurp version))]
    (json
     {:sha sha
      :source (str "https://github.com/geni/geni-gedcom/tree/" sha)
      :version version})))

(defroutes system-routes
  (GET "/status" [] (status)))