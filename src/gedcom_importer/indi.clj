(ns gedcom-importer.indi
  (:require [clojure.string :as string]))

(defmulti to-geni first)

(defmethod to-geni "NAME"
  [record]
  (let [name (-> record second first :data)
        [[first-name & middles] [last-name suffix] :as all]
        (split-with
          #(not (.startsWith % "/"))
          (map first (re-seq #"(/[^/]*\*?/\*?|[^* ]+\*?)"
                             (string/replace-first name #"/" " /"))))]
    {:first_name first-name
     :middle_name (when middles (string/join " " middles))
     :last_name (when last-name (last (re-find #"/(.*)/" last-name)))
     :suffix suffix}))

(defmethod to-geni :default [_] nil)

(defn indi-to-geni [records]
  (reduce merge (map to-geni records)))
