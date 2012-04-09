(ns gedcom-importer.names
  (:require [clojure.string :as string]))

(defn name-components
  "Parses a name from a GEDCOM personal name structure into
  a map of components first, middle, last, and suffix."
  [name]
  (let [[[first-name & middles] [last-name suffix] :as all]
        (split-with #(not (.startsWith % "/"))
                    (map first (re-seq #"(/[^/]*\*?/\*?|[^* ]+\*?)"
                                       (string/replace-first name #"/" " /"))))]
    {:first first-name
     :middle (when middles (string/join " " middles))
     :last (when last-name (last (re-find #"/(.*)/" last-name)))
     :suffix suffix}))
