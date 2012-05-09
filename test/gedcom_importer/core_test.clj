(ns gedcom-importer.core_test
  (:require [clojure.test :refer :all]
            [gedcom-importer.core :as g]))

(deftest replace-existing-test
  (is (= {:partners ["profile-1" "P2"]}
         (g/replace-existing {:profiles {"P1" "profile-1"}} {:partners ["P1" "P2"]}))))