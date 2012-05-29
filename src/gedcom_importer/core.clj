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

(defn process-profiles [records processed processing fam-id fam]
  (let [results (for [profile-id (mapcat val fam)]
                  (if (or (contains? (:profiles processed) (keyword profile-id))
                          (contains? (:profiles processing) profile-id))
                    [[profile-id nil]]
                    (let [[fams record] (indi/indi (lookup-label records profile-id))]
                      [[profile-id record] (remove #{fam-id} fams)])))]
    [(into {} (map first results))
     (mapcat second results)]))

(defn update-existing [processed tree]
  (assoc-in tree [:profiles]
            (into
             {}
             (for [[id record] (:profiles tree)]
               (if (nil? record)
                 (if-let [profile (get-in processed [:profiles (keyword id)])]
                   [id profile]
                   nil)
                 [id record])))))

(defn workaround [fam]
  (if (:partners fam)
    fam
    (assoc fam :partners [])))

(defn unions [records fams]
  (for [fam fams]
    [fam (workaround (fam/fam (lookup-label records fam)))]))

(defn ready? [processed tree]
  (let [{processed-unions :unions, processed-profiles :profiles} processed
        {:keys [profiles unions]} tree]
    (or (<= 100 (+ (count processed-unions) (count unions)))
        (<= 100 (+ (count processed-profiles) (count profiles))))))

(defn import-gedcom
  [file label token]
  (let [records (ged/parse-gedcom-records file)]
    (loop [processed {:profiles {(keyword label) (:id (geni/read "/profile" {:token token}))}}
           processing {}
           fams-to-process (->> label (lookup-label records) indi/indi first)]
      (when (seq fams-to-process)
        (let [fams (unions records fams-to-process)
              results (map #(apply process-profiles records processed processing %) fams)
              unprocessed (mapcat second results)
              tree {:unions (into {} fams)
                    :profiles (apply merge (map first results))}]
          (if (or (ready? processing tree) (not (seq unprocessed)))
            (do (spit "ftest" (str "importing\n\n" (with-out-str (pprint processing))) :append true)
                (spit "ftest" (str "\n\ntree\n\n" (with-out-str (pprint tree))) :append true)
                (spit "ftest" (str "\n\nunprocessed\n\n" (pr-str unprocessed)) :append true)
                (let [g (import-group (update-existing processed processing) token)]
                  (spit "ftest" (str "\n\nunprocessed\n\n" (with-out-str (pprint g))) :append true)
                  (recur (adjoin g processed) tree unprocessed)))
            (recur processed (adjoin processing tree) unprocessed)))))))