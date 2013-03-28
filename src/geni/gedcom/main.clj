(ns geni.gedcom.main
  (:gen-class)
  (:require [geni.gedcom.import :refer [import-gedcom]]
            [geni.gedcom.common :refer [to-geni]]
            [geni.gedcom.web.common :refer [config]]
            [gedcom.core :refer [parse-gedcom]]
            [geni.core :as geni]))

(defn -main [& [token start gedfile]]
  (binding [geni/*base* (config "url")
            geni/*insecure* (config "insecure")
            geni/*basic-auth* (config "basicauth")]
    (dorun (import-gedcom (parse-gedcom gedfile to-geni) start token))))