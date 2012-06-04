(ns geni.gedcom.import_test
  (:require [clj-gedcom.core :refer [parse-gedcom]]
            [geni.gedcom.common :refer [to-geni]]
            [useful.map :refer [map-vals merge-in]]
            [geni.gedcom.import :refer [walk-gedcom]]
            [clojure.test :refer :all]))

(defn gedcom [file]
  (map-vals (parse-gedcom (str (System/getProperty "user.dir") "/gedcoms/" file)) to-geni))

(defn one-tree [batches]
  (reduce merge-in batches))

(defn union-profiles [records]
  (set (apply concat (mapcat vals (vals (:unions records))))))

(deftest walk-gedcom-test
  (let [records (walk-gedcom (gedcom "cycle.ged") "@I3@")
        tree (one-tree records)
        unions (union-profiles tree)]
    (testing "All profiles mentioned in unions are in profiles."
      (is (not (some (comp not unions) (keys (:profiles tree))))))
    (testing "Number of steps."
      (is (= 2 (count records))))))