(ns gedcom-importer.fam
  (:require [gedcom-importer.to-geni :refer [to-geni get-data]]
            [useful.utils :refer [adjoin]]))

;; The CHIL tag is the only one that there should ever be
;; more than one of inside of a FAM record.
(defmethod to-geni "CHIL" [record]
  {:children (map :data (second record))})

(defmethod to-geni "HUSB" [record]
  {:partners [(get-data record)]})

(defmethod to-geni "WIFE" [record]
  {:partners [(get-data record)]})

(defn fam
  "Parse a FAM record into a map of :children
   and :partners."
  [record]
  (reduce adjoin (map to-geni record)))
