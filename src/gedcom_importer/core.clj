(ns gedcom-importer.core
  (:require [clj-gedcom.core :refer [parse-gedcom]]
            [useful.utils :refer [adjoin]]
            [useful.map :refer [merge-in update update-each map-to map-vals-with-keys]]
            [gedcom-importer.fam :refer [fam indi-ids]]
            [gedcom-importer.indi :refer [indi fam-ids]]
            [geni.core :as geni]))

(defn import-tree
  "Import a map of profiles and unions. Replace all nodes that have already been assigned ids with
  the id. Return a new map of labels to ids. Note that /profiles/import_tree has a size limit for
  unions and profiles which is 100 by default."
  [token ids tree]
  (let [tree (update-each tree [:profiles :unions]
                          map-vals-with-keys
                          (fn [k v]
                            (get ids k v)))
        new-ids (geni/write "/profiles/import_tree"
                            tree
                            {:access_token token
                             :only_ids 1})]
    (merge-in ids new-ids)))

(def ^:dynamic *max-batch-size* 100)

(defn follow-fam
  "Follow a single FAM record and add all the linked INDI records to the current batch, splitting
  the batch every time we have encountered more than *max-batch-size* profiles."
  [state fam-id]
  (let [union (fam (get-in state [:records fam-id]))
        indi-ids (indi-ids union)
        followed (into (:followed state) (conj indi-ids fam-id))
        profile-count (+ (:profile-count state) (count indi-ids))
        profiles (map-to (fn [indi-id]
                           (if (contains? followed indi-id)
                             {}
                             (indi (get-in state [:records indi-id]))))
                         indi-ids)
        tree {:profiles profiles, :unions {fam-id union}}
        state (-> state
                  (assoc :followed followed)
                  (update :to-follow into (remove followed
                                                  (mapcat fam-ids (vals profiles)))))]
    (if (>= profile-count *max-batch-size*)
      (-> state
          (update :batches conj (:batch state))
          (assoc :batch tree
                 :profile-count 0))
      (-> state
          (update :batch merge-in tree)
          (assoc :profile-count profile-count)))))

(defn prepare-gedcom
  "Prepare a set of GEDCOM records for import by walking over the graph of INDI and FAM records and
  splitting the records into batches for import."
  [records label id]
  (loop [state {:ids {label id}
                :followed #{}
                :profile-count 0
                :to-follow (fam-ids (indi (get records label)))}]
    (if-let [fams (seq (:to-follow state))]
      (recur
       (reduce follow-fam
               (dissoc state :to-follow)
               fams))
      (conj (:batches state)
            (:batch state)))))

(defn import-gedcom
  "Import the given GEDCOM file using the Geni API. The provided label identifies yourself in the
  GEDCOM. Token is expected to be a Geni OAuth access token."
  [file label token]
  (let [id (:id (geni/read "/profile" {:access_token token}))
        records (parse-gedcom file)]
    (reduce (partial import-tree token) {}
            (prepare-gedcom records label id))))
