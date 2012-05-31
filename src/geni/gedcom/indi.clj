(ns geni.gedcom.indi
  (:require [clojure.string :as string]
            [geni.gedcom.common :refer [to-geni get-data event]]
            [useful.utils :as utils]))

;; Parse the name of an individual into first_name, middle_name
;; last_name, and suffix parts. The first name is always the very
;; first part of a name. The middle name is everything after the
;; first name up to the first slash (/) that delimits the last
;; name. After the next / is the suffix.
(defmethod to-geni "NAME"
  [record]
  (let [name (get-data record)
        [[first-name & middles] [last-name suffix]]
        (split-with
          #(not (.startsWith % "/"))
          (map first (re-seq #"(/[^/]*\*?/\*?|[^* ]+\*?)"
                             (string/replace-first name #"/" " /"))))]
    {:first_name first-name
     :middle_name (when middles (string/join " " middles))
     :last_name (when last-name (last (re-find #"/(.*)/" last-name)))
     :suffix suffix}))

;; BIRT, DEAT, BURI, and BAPM all have the same general structure.
;; The difference between them is what key we put the results
;; under for passing to the API. Therefore, these methods are
;; very simple.
(defmethod to-geni "BIRT" [record] (event record :birth))
(defmethod to-geni "DEAT" [record] (assoc (event record :death) :is_alive false))
(defmethod to-geni "BAPM" [record] (event record :baptism))
(defmethod to-geni "BURI" [record] (event record :burial))

(defmethod to-geni "FAMS" [record]
  {:partner (map :data (second record))})

(defmethod to-geni "FAMC" [record]
  {:child (map :data (second record))})

(defmethod to-geni "SEX" [record]
  {:gender (case (get-data record)
             "M" "male"
             "F" "female"
             nil)})

(defn indi
  "Parse an INDI record and return a map suitable for passing
  to the Geni API."
  [records]
  (let [parsed (reduce merge (map to-geni records))]
    (if (contains? parsed :is_alive)
      parsed
      (assoc parsed :is_alive true))))

(defn fam-ids
  "Return the FAM links for an already parsed INDI record."
  [indi]
  (when (map? indi)
    (mapcat indi [:child :partner])))

(defn without-fams
  "Return the INDI record with FAM links removed."
  [indi]
  (dissoc indi :child :partner))