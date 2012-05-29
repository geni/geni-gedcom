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
  (geni/write "/profiles/import_tree" (assoc tree :token token)))

(defn geni-id [s]
  (when s (last (.split s "/"))))

(defn process-profiles [records processed processing fam-id fam]
  (let [results (for [profile-id (mapcat val fam)
                      :when (not (contains? (:profiles processing) profile-id))]
                  (if-let [processed-id (geni-id (get-in processed [:profiles (keyword profile-id)]))]  
                    [[profile-id processed-id]]
                    (let [[fams record] (indi/indi (lookup-label records profile-id))]
                      [[profile-id record] (remove #{fam-id} fams)])))]
    [{:profiles (into {} (map first results))
      :unions {fam-id fam}}
     (mapcat second results)]))

(defn workaround [fam]
  (if (:partners fam)
    fam
    (assoc fam :partners [])))

(defn partition-fams
  "Returns a seq of groups of unions whose cumulative number of profiles is less than 100."
  [unions]
  (loop [max 0 groups [] acc [] [group & rest] unions]
    (let [max (+ max (count (mapcat val (second group))))]
      (cond
       (nil? group) (conj groups acc)
       (>= max 100) (recur 0 (conj groups acc) [group] rest)
       :else (recur max groups (conj acc group) rest)))))

(defn unions [records fams]
  (partition-fams
   (for [fam fams]
     [fam (workaround (fam/fam (lookup-label records fam)))])))

(defn prepare-group [records processed group]
  (loop [processing {}
         unprocessed []
         [[fam-id record] & rest] group]
    (if fam-id
      (let [[tree links] (process-profiles records processed processing fam-id record)]
        (recur (merge-with merge processing tree) (concat unprocessed links) rest))
      [processing unprocessed])))

(defn debug [message item]
  (spit "ftest" (str message (with-out-str (pprint item))) :append true)
  item)

(defn import-groups [records token processed groups]
  (loop [processed processed
         unprocessed []
         [group & rest] groups]
    (if group
      (let [[tree links] (prepare-group records processed group)]
        (debug "\n\nprepared\n\n" tree)
        (recur (merge-with merge processed (debug "\n\nimporting\n\n" (import-group tree token)))
               (concat unprocessed links)
               rest))
      [processed unprocessed])))

(defn import-gedcom
  [file label token]
  (let [records (ged/parse-gedcom-records file)]
    (loop [processed {:profiles {(keyword label) (:id (geni/read "/profile" {:token token}))}}
           fams-to-process (->> label (lookup-label records) indi/indi first)]
      (when (seq fams-to-process)
        (let [fam-groups (unions records fams-to-process)
              [done unprocessed] (import-groups records token processed fam-groups)]
          (recur (merge-with merge done processed) unprocessed))))))