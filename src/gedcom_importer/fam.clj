(ns gedcom-importer.fam
  (:require [gedcom-importer.to-geni :refer [to-geni get-data]]))

;; The CHIL tag is the only one that there should ever be
;; more than one of.
(defmethod to-geni "CHIL" [record]
  {:children (map :data (second record))})

(defmethod to-geni "HUSB" [record]
  {:husband (get-data record)})

(defmethod to-geni "WIFE" [record]
  {:wife (get-data record)})

(defn fam
  "Parse a FAM record into a map of :children,
  :wife, and :husband."
  [record]
  (reduce merge (map to-geni record)))
