(ns geni.gedcom.import_test
  (:require [clj-gedcom.core :refer [parse-gedcom]]
            [geni.gedcom.common :refer [to-geni]]
            [useful.map :refer [map-vals merge-in]]
            [geni.gedcom.import :refer [walk-gedcom in-batches]]
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

(deftest in-batches-test
  (let [records (walk-gedcom (gedcom "private/steadman-BloodTree.ged") "@I0@")]
    (testing "Number of profiles stays below specified number."
      (is (every? #(<= (count %) 30) (map :profiles (in-batches 30 records)))))
    (testing "Number of unions stays below specified number."
      (is (every? #(<= (count %) 10) (map :unions (in-batches 10 records)))))))