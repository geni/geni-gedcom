(ns geni.gedcom.common)

(defmulti to-geni
  "Parse pieces of GEDCOM records into items consumable by the Geni API."
  first)

(defmethod to-geni :default [_] nil)

(def ^{:doc "Convenience function that gets the :data from the first subrecord of a record."}
  get-data
  (comp :data first second))
