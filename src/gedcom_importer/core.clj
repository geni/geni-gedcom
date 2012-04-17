(ns gedcom-importer.core
  (:require [clj-gedcom.core :as ged]
            [gedcom-importer.fam :as fam]
            [gedcom-importer.indi :as indi]))

(defn lookup-label [records id]
  (first (filter #(= id (:label %)) records)))

(defn import-record [records to to-label record type token]
  (when (= to-label record)
    (let [[fams params] (indi/indi (lookup-label records record))]
      [(:id (geni/write (str "/" to "/add-" (name type))
                        (assoc params :token token)))
       record
       fams])))

;; After we've added a FAM, we need to remove any cross
;; references to that FAM from the to-be-processed fams.
;; If we don't do this, we will end up in an infinite loop
;; importing the same FAM over and over again.
(defn remove-old [results current]
  (for [[id {:keys [spouse child]} :as all] results
        :when (not (or (some #{current} spouse)
                       (some #{current} child)))]
    all))

(defn import-fam [records to to-label fam type token]
  (let [{:keys [children husband wife]} (fam/fam (lookup-label records fam))]
    (remove-old
      (concat
        [(import-record records to to-label husband
                        (if (= :type :spouse)
                          :partner
                          :parent)
                        token)
         (import-record records to to-label wife
                        (if (= :type :spouse)
                          :partner
                          :parent)
                        token)]
        (doall (map #(import-record records to to-label %
                                    (if (= :type :spouse)
                                      :child
                                      :sibling)
                                    token)
                    children)))
      fam)))

(defn import-gedcom
  "Import a GEDCOM file. ged is the GEDCOM file
  itself. label is the label of the INDI record
  corresponding with the Geni account being used
  to add family."
  [file label token]
  (let [records (ged/parse-gedcom-records file)
        import (fn [[id label fam] item]
                 (import-fam records id label))]
    (loop [fams [[(:id (geni/read "/profile" {:token token}))
                  label
                  (->> label (lookup-label records) indi/indi first)]]]
      )))
