(ns geni.gedcom.web.models.api
  (:require [geni.gedcom.web.common :refer [catch-exception-string config]]
            [geni.gedcom.import :as import]
            [geni.gedcom.common :refer [to-geni]]
            [geni.core :as geni]
            [gedcom.core :refer [parse-gedcom]]
            [clojure.string :refer [join]]
            [clojure.tools.logging :refer [info error]]))

(def cache (atom {:id 0}))

(defn cache-records
  "Cache records that may be imported in the near future."
  [cache records record-count token]
  (let [id (inc (:id cache))]
    (assoc cache
      token {:id id
             :records records
             :profile-count record-count
             :progress 0
             :status :waiting}
      :id id)))

(defn update-status [token v]
  (swap! cache assoc-in [token :status] v))

(defn check-progress [token]
  (let [cached (get @cache token)]
    (merge {:profile_count (:profile-count cached)}
           (select-keys cached [:progress :status]))))

(defn remove-records
  "If the cached id is the same as id, remove the records
   associated with token."
  [cache token id]
  (if (= id (get-in cache [token :id]))
    (update-in cache [token] dissoc :records)
    cache))

(defn future-clean
  "Setup a thread that waits for 5 minutes and then removes
   cached records under token if the associated id is the same
   as id."
  [token id]
  (future
    (Thread/sleep 300000)
    (swap! cache remove-records token id)))

(defn extract-info
  "Extract first name, middle name, last name, and birth date info
   out of a record."
  [record]
  (update-in (select-keys record [:first_name :middle_name :last_name :birth])
             [:birth]
             :date))

(defn push-to-end
  "Same as compare, but if either key is nil, do the opposite
   of what compare would do."
  [k1 k2]
  (let [compared (compare k1 k2)]
    (if (and k1 k2)
      compared
      (- compared))))

(defn names
  "Takes gedcom records and returns a map of names from INDI
   records to their corresponding ids."
  [records]
  (sort-by
   :first_name
   push-to-end
   (reduce (fn [acc [k v]]
             (if (= :indi (:record-type v))
               (conj acc (assoc (extract-info v) :id k))
               acc))
           []
           records)))

(defn prepare-gedcom
  "Parses an uploaded gedcom, stores the parsed records in the cache,
   sets up a thread to to clear the records from the cache after 5
   minutes, and returns either a map with an :error key if an exception
   occurred or a map with a :names key keyed to a map of GEDCOM labels
   to names."
  [gedcom token]
  (let [results (catch-exception-string (str "Error parsing gedcom for " token)
                  (parse-gedcom (:tempfile gedcom) to-geni))]
    (if (map? results)
      (let [names (names results)]
        (future-clean token (:id (swap! cache cache-records results (count names) token)))
        (info (str "Successfully parsed gedcom records for " token))
        {:names names})
      {:error results})))

(defn import-records
  "Call import-gedcom respecting settings in gedcom.properties."
  [records id token]  
  (.start
   (Thread.
    (fn []
      (binding [geni/*base* (config "url")
                geni/*insecure* (Boolean/valueOf (config "insecure"))]
        (let [result-seq (import/import-gedcom records id token)]
          (info (str "Importing profiles for " token))
          (update-status token :importing)
          (doseq [[progress x] result-seq]
            (swap! cache assoc-in [token :progress] progress))
          (update-status token :done)
          (info (str "Imported profiles for " token)))))))
  {:profile_count (get-in @cache [token :profile-count])})

(defn import-gedcom
  "Initiates an import with records previously cached against token.
   The import begins with id in the GEDCOM and walks out from there.
   If an error occurs, returns a map with an :error key, otherwise
   returns the number of profiles to be imported. The :progress key
   in the map associated with token in the cache will be updated at
   every interation of the import."
  [id token]
  (if-let [records (get-in @cache [token :records])]
    (do (swap! cache update-in [token] dissoc :records)
        (import-records records id token))
    (let [message (str "No records associated with token " token)]
      (error message)
      {:error message})))

