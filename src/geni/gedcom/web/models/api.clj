(ns geni.gedcom.web.models.api
  (:require [geni.gedcom.web.common :refer [catch-exception-string config]]
            [geni.gedcom.import :refer [import-gedcom]]
            [geni.gedcom.common :refer [to-geni]]
            [geni.core :as geni]
            [clojure.string :refer [join]]))

(def cache (atom {:id 0}))

(defn cache-records
  "Cache records that may be imported in the near future."
  [cache records token]
  (let [id (inc (:id cache))]
    (assoc cache
      token {:id id, :records records}
      :id id)))

(defn remove-records
  "If the cached id is the same as id, remove the records
   associated with token."
  [cache token id]
  (if (= id (get-in cache [token :id]))
    (dissoc cache token)
    cache))

(defn future-clean
  "Setup a thread that waits for 5 minutes and then removes
   cached records under token if the associated id is the same
   as id."
  [token id]
  (future
    (Thread/sleep 300000)
    (swap! cache remove-records token id)))

(defn names
  "Takes gedcom records and returns a map of names from INDI
   records to their corresponding ids."
  [records]
  (reduce (fn [acc [k v]]
            (if (= :indi (:record-type v))
              (assoc acc
                k {:name (join " " (filter identity
                                           ((juxt :first_name
                                                  :middle_name
                                                  :last_name)
                                            v)))})
              acc))
          {}
          records))

;; Waiting on Scott to add count keys to import_tree's results.
(defn count-profiles
  "Count the number of profiles returned by the Geni API."
  [results]
  (count (filter #(.startsWith (val %) "profile") results)))

(defn import-records
  "Call import-gedcom respecting settings in gedcom.properties."
  [records id token]
  (binding [geni/*base* (config "url")
            geni/*insecure* (Boolean/valueOf (config "insecure"))]
    (catch-exception-string (import-gedcom records id token))))
