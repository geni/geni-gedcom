(ns geni.gedcom.import
  (:require [gedcom.core :refer [parse-gedcom]]
            [useful.utils :refer [adjoin]]
            [useful.map :refer [merge-in update update-each map-to map-vals-with-keys]]
            [geni.gedcom.fam :refer [fam indi-ids]]
            [geni.gedcom.indi :refer [indi fam-ids without-fams]]
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
        results (geni/write "/profiles/import_tree"
                            tree
                            {:access_token token
                             :only_ids 1})]
    (merge-in ids (get results "imported"))))

(def ^:dynamic *max-batch-size* 100)

(defn follow
  "Follow a single INDI record and add all the linked FAM records to the current batch, splitting
  the batch every time we have encountered more than *max-batch-size* profiles."
  [state indi-id]
  (let [profile (indi (get-in state [:records indi-id]))
        followed (:followed state)
        fam-ids (remove followed (fam-ids profile))
        unions (map-to #(fam (get-in state [:records %])) fam-ids)
        tree {:unions unions
              :profiles {indi-id (without-fams profile)}}
        state (-> state
                  (update :followed into (conj fam-ids indi-id))
                  (update :to-follow into (remove followed
                                                  (mapcat indi-ids (vals unions)))))
        batch (merge-in (:batch state) tree)]
    (if (or (< *max-batch-size* (count (:unions batch)))
            (< *max-batch-size* (count (:profiles batch))))
      (-> state ;; the batch is too big, start a new batch
          (assoc :batch tree)
          (update :batches conj (:batch state)))
      (assoc state :batch batch))))

(defn prepare-gedcom
  "Prepare a set of GEDCOM records for import by walking over the graph of INDI and FAM records and
  splitting the records into batches for import."
  [records label]
  (loop [state {:records records
                :followed #{}
                :to-follow [label]
                :batches []}]
    (if-let [indi-ids (seq (:to-follow state))]
      (recur
       (reduce follow
               (dissoc state :to-follow)
               indi-ids))
      (conj (:batches state)
            (:batch state)))))

(defn import-gedcom
  "Import the given GEDCOM file using the Geni API. The provided label identifies yourself in the
  GEDCOM. Token is expected to be a Geni OAuth access token."
  [file label token]
  (let [id (get (geni/read "/profile" {:access_token token}) "id")
        records (parse-gedcom file)]
    (reduce (partial import-tree token)
            {label id}
            (prepare-gedcom records label))))
