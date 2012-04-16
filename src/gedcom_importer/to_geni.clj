(ns gedcom-importer.to-geni)

(defmulti to-geni
  "Parse pieces of GEDCOM records into items consumable
  by the Geni API."
  first)

(defmethod to-geni :default [_] nil)

(def get-data
  "Convenience function that gets the :data from
  the first subrecord of a record."
  (comp :data first second))
