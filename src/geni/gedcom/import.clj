(ns geni.gedcom.import
  (:require [gedcom.core :refer [parse-gedcom]]
            [useful.utils :refer [queue]]
            [useful.seq :refer [glue lazy-loop]]
            [useful.map :refer [merge-in update-each map-to map-vals map-vals-with-keys remove-vals update-each]]
            [geni.gedcom.common :refer [to-geni profile-ids union-ids]]
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

(defn walk-gedcom
  "Walk GEDCOM records starting at label by walking over the graph of INDI and FAM records.
  Returns a list of the profiles and unions encountered at each step."
  [records label]
  (lazy-loop [to-follow (queue [label])
              followed? #{label}]
    (when-let [profile-id (first to-follow)]
      (let [union-ids   (remove followed? (union-ids (get records profile-id)))
            unions      (remove-vals
                         (map-vals
                          (map-to records union-ids)
                          #(update-each % [:children :partners] (partial filter records)))
                         #(< (-> % profile-ids count) 2))
            profile-ids (mapcat profile-ids (vals unions))
            profiles    (map-to records profile-ids)

            new-follow-queue (into (pop to-follow) (remove followed? profile-ids))
            new-followed (into followed? (concat union-ids profile-ids))]
        (if (every? empty? [unions profiles])
          (recur new-follow-queue new-followed)
          (cons {:unions unions, :profiles profiles}
                (lazy-recur new-follow-queue new-followed)))))))

(defn in-batches
  "Combine the steps produced by walk-gedcom into batches of no more than n profiles or unions."
  [n steps]
  (glue merge-in
        (constantly true)
        (fn [batch]
          (some #(< n (count (get batch %)))
                [:unions :profiles]))
        steps))

(defn import-gedcom
  "Import the given GEDCOM records using the Geni API. The provided label identifies yourself in the
  GEDCOM. Token is expected to be a Geni OAuth access token."
  [records label token]
  (reduce (partial import-tree token)
          {label (get (geni/read "/profile" {:access_token token}) "id")}
          (in-batches *max-batch-size*
                      (walk-gedcom records label))))
