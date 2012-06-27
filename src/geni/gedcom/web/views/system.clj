(ns geni.gedcom.web.views.system
  (:require [compojure.core :refer [defroutes GET]]
            [clojure.string :refer [trim-newline]]
            [clojure.java.io :refer [resource]]
            [noir.response :refer [json]]))

(defn status []
  (let [sha (when-let [sha (resource "gitsha")]
              (trim-newline (slurp sha)))
        version (when-let [version (resource "leinversion")]
                  (trim-newline (slurp version)))]
    (json
     {:sha sha
      :source (str "https://github.com/geni/geni-gedcom/tree/" sha)
      :version version})))

(defroutes system-routes
  (GET "/status" [] (status)))