(ns gedcom-importer.core
  (:require [clj-gedcom.core :as ged]
            [useful.utils :as utils]
            [geni.core :as geni]
            [gedcom-importer.fam :as fam]
            [gedcom-importer.indi :as indi]))

(defn lookup-label [records id]
  (first (filter #(= id (:label %)) records)))

(defn import-record [records to to-label record type gender token]
  (when-not (or (nil? record) (= to-label record))
    (let [[fams params] (indi/indi (lookup-label records record))]
      (when (seq fams)
        [(:id (geni/write (str "/" to "/add-" (name type))
                          (merge-with merge
                                      (assoc params :token token)
                                      {:profile {:gender gender}})))
         record
         fams]))))

;; After we've added a FAM, we need to remove any cross
;; references to that FAM from the to-be-processed fams.
;; If we don't do this, we will end up in an infinite loop
;; importing the same FAM over and over again.
;; TODO: Get rid of this because it is probably stupid and slow.
(defn remove-old [results current]
  (for [[id label {:keys [spouse child]} :as all] results
        :let [spouse (remove #{current} spouse)
              child (remove #{current} child)]
        :when (or (seq spouse) (seq child))]
    [id label {:spouse spouse, :child child}]))

(defn import-fam [records to to-label fam type token]
  (let [{:keys [children husband wife]} (fam/fam (lookup-label records fam))
        spouse? (= type :spouse)]
    (remove-old
      (concat
        [(import-record records to to-label husband
                        (if spouse?
                          :partner
                          :parent)
                        "male" token)
         (import-record records to to-label wife
                        (if spouse?
                          :partner
                          :parent)
                        "female" token)]
        (map #(import-record records to to-label %
                             (if spouse?
                               :child
                               :sibling)
                             nil token)
             children))
      fam)))

(defn import-type [records id label type token]
  (fn [fam]
    (import-fam records id label fam type token)))

(defn import-for [records token]
  (fn [[id label fam]]
    (seq
      (concat (mapcat (import-type records id label :spouse token)
                      (:spouse fam))
              (mapcat (import-type records id label :child token)
                      (:child fam))))))

(defn import-gedcom
  "Import a GEDCOM file. ged is the GEDCOM file
  itself. label is the label of the INDI record
  corresponding with the Geni account being used
  to add family."
  [file label token]
  (let [records (ged/parse-gedcom-records file)]
    (loop [fam [[(:id (geni/read "/profile" {:token token}))
                 label
                 (->> label (lookup-label records) indi/indi first)]]]
      (when (seq (doall fam))
        (prn fam)
        (recur (mapcat (import-for records token) (filter identity fam)))))))
