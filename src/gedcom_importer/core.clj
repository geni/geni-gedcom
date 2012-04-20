(ns gedcom-importer.core
  (:require [clj-gedcom.core :as ged]
            [clojure.string :as string]
            [useful.utils :as utils]
            [geni.core :as geni]
            [gedcom-importer.fam :as fam]
            [gedcom-importer.indi :as indi]))

(defn lookup-label [records id]
  (first (filter #(= id (:label %)) records)))

(def split (fnil string/split ""))

(defn get-union [data]
  (-> data :unions first (split #"/") last))

(defn import-record [records to to-label record type token]
  (when-not (or (nil? record) (= to-label record))
    (let [[fams params] (indi/indi (lookup-label records record))]
      (when (seq fams)
        (let [written (geni/write (str "/" to "/add-" (name type))
                                  (assoc params :token token))]
         [[(:id written) record fams] (get-union written)])))))

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
      (if spouse?
        (let [[husband-result h-union] (import-record records to to-label husband :partner token)
              [wife-result w-union]    (import-record records to to-label wife :partner token)
              to                       (or w-union h-union (first husband-result) (first wife-result) to)]
          (concat
            [husband-result wife-result
             (map #(first (import-record records to to-label % :child token))
                  children)]))
        (concat
          (map first [(import-record records to to-label husband :parent token)
                      (import-record records to to-label wife :parent token)])
          (map #(first (import-record records to to-label % :sibling token))
               children)))
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
        (recur (mapcat (import-for records token) (filter identity fam)))))))
