(ns gedcom-importer.core
  (:require [clj-gedcom.core :as ged]
            [useful.utils :refer [adjoin]]
            [geni.core :as geni]
            [gedcom-importer.fam :as fam]
            [gedcom-importer.indi :as indi]
            [clojure.pprint :refer [pprint]]))

(defn lookup-label [records id]
  (first (filter #(= id (:label %)) records)))

(defn import-group
  "Import a map of profiles and unions."
  [tree token]
  (geni/write "/profiles/import-tree" (assoc tree :token token)))

(defn lookup-existing [processed id]
  (get-in processed [:profiles id]))

(defn process-profiles [records processed processing fam-id fam]
  (let [results (for [profile-id (mapcat val fam)
                      :when (and (not (contains? (:profiles processing) profile-id))
                                 (not (.startsWith profile-id "profile-")))
                      :let [[fams record] (indi/indi (lookup-label records profile-id))]]                  
                  [[profile-id record] (remove #{fam-id} fams)])]
    [(into {} (map first results))
     (mapcat second results)]))

(defn replace-existing [processed fam]
  (into {}
        (for [[k v] fam
              :let [v (map #(or (lookup-existing processed %) %) v)]]
          [k v])))

(defn unions [records processed fams]
  (for [fam fams]
    [fam (replace-existing processed (fam/fam (lookup-label records fam)))]))

(defn ready? [processed tree]
  (let [{processed-unions :unions, processed-profiles :profiles} processed
        {:keys [profiles unions]} tree]
    (or (<= 100 (+ (count processed-unions) (count unions)))
        (<= 100 (+ (count processed-profiles) (count profiles))))))

(defn import-gedcom
  [file label token]
  (let [records (ged/parse-gedcom-records file)]
    (loop [processed {:profiles {label (:id (geni/read "/profile" {:token token}))}}
           processing {}
           fams-to-process (->> label (lookup-label records) indi/indi first)]
      (when (seq fams-to-process)
        (let [fams (unions records processed fams-to-process)
              results (map #(apply process-profiles records processed processing %) fams)
              unprocessed (mapcat second results)
              tree {:unions (into {} fams)
                    :profiles (apply merge (map first results))}]
          (if (or (ready? processing tree) (not (seq unprocessed)))
            (recur (merge-with merge (import-group processing token) processed) tree unprocessed)
            (recur processed (merge-with merge processing tree) unprocessed)))))))
