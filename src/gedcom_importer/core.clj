(ns gedcom-importer.core
  (:require [clj-gedcom.core :as ged]
            [useful.utils :refer [adjoin]]
            [geni.core :as geni]
            [gedcom-importer.fam :as fam]
            [gedcom-importer.indi :as indi]
            [clojure.pprint :refer [pprint]]))

(defn import-group
  "Import a map of profiles and unions."
  [tree token]
  (geni/write "/profiles/import_tree" (assoc tree :token token)))

(defn geni-id [s]
  (when s (last (.split s "/"))))

(defn process-profiles [records processed processing fam-id fam]
  (let [results (for [profile-id (fam/indis fam)
                      :when (not (contains? (:profiles processing) profile-id))]
                  (if-let [processed-id (geni-id (get-in processed [:profiles (keyword profile-id)]))]  
                    [[profile-id processed-id]]
                    (let [record (indi/indi (get records profile-id))]
                      [[profile-id (indi/without-fams record)]
                       (remove #{fam-id} (indi/fams record))])))]
    [{:profiles (into {} (map first results))
      :unions {fam-id fam}}
     (mapcat second results)]))

(defn partition-fams
  "Returns a seq of groups of label/union pairs whose cumulative number of profiles is less than 100."
  [records fams]
  (loop [num-profiles 0, groups [], group [], fams fams]
    (if (seq fams)
      (let [fam-id (first fams)
            union (fam/fam (get records fam-id))
            profile-count (count (fam/indis union))
            new-num-profiles (+ num-profiles profile-count)]
        (cond
         (<= profile-count 1) (recur num-profiles groups group (rest fams))
         (>= new-num-profiles 100) (recur 0 (conj groups group) [] fams)
         :else (recur new-num-profiles groups (conj group [fam-id union]) (rest fams))))
      (conj groups group))))

(defn prepare-group [records processed group]
  (loop [processing {}
         unprocessed []
         group group]
    (if (seq group)
      (let [[fam-id union] (first group)
            [tree links] (process-profiles records processed processing fam-id union)]
        (recur (merge-with merge processing tree)
               (concat unprocessed links)
               (rest group)))
      [processing unprocessed])))

(defn debug [message item]
  (spit "ftest" (str message (with-out-str (pprint item))) :append true)
  item)

(defn import-groups [records token processed fams]
  (loop [processed processed
         unprocessed []
         groups (partition-fams records fams)]
    (if (seq groups)
      (let [[tree links] (prepare-group records processed (first groups))]
        (debug "\n\nprepared\n\n" tree)
        (recur (merge-with merge processed (debug "\n\nimporting\n\n" (import-group tree token)))
               (concat unprocessed links)
               (rest groups)))
      [processed unprocessed])))

(defn import-gedcom
  [file label token]
  (let [records (ged/parse-gedcom file)]
    (loop [processed {:profiles {(keyword label) (:id (geni/read "/profile" {:token token}))}}
           fams-to-process (indi/fams (indi/indi (get records label)))]
      (when (seq fams-to-process)
        (let [[done unprocessed] (import-groups records token processed fams-to-process)]
          (recur (merge-with merge done processed) unprocessed))))))

