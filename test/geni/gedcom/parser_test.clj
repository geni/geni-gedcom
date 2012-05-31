(ns geni.gedcom.parser-test
  (:require [clojure.test :refer :all]
            [clj-gedcom.core :refer [parse-gedcom]]
            [geni.gedcom.fam :refer [fam]]
            [geni.gedcom.indi :refer [indi]]))

(def records (parse-gedcom (str (System/getProperty "user.dir") "/gedcoms/indi.ged")))

(deftest event-test
  (let [person (indi (records "@I0@"))
        fam (fam (records "@I0@"))
        event-data {:date {:year 1994, :day 2, :month 2, :circa false}
                    :location {:place_name "The Library"
                               :country "Westeros"
                               :state "Arkham"
                               :city "Metropolis"}}]
    (testing "Profile events are parsed properly."
      (apply = event-data (map person [:death :birth :burial :baptism])))
    (testing "Union events are parsed properly."
      (apply = event-data (map fam [:marriage :divorce])))))

(deftest name-test
  (let [person (indi (records "@I0@"))]
    (testing "Name is parsed into it's correct portion."
      (is (= {:first_name "Mary"
              :middle_name "Marrison Goosian"
              :last_name "Swanson"
              :suffix "jr"}
             (select-keys person [:first_name :middle_name :last_name :suffix]))))))

(deftest gender-test
  (testing "SEX is parsed into :gender male or female."
    (is (= "male" (-> "@I2@" records indi :gender)))
    (is (= "female" (-> "@I0@" records indi :gender)))))

(deftest date-test
  (let [expected {:year 1994 :day 2 :month 2 :circa false}
        get-date (fn [ged person] (get-in (indi (ged person)) [:birth :date]))]
    (is (apply = expected
               (map (partial get-date records)
                    ["@I2@" "@I3@" "@I4@" "@I5@" "@I6@"])))))

(deftest union-test
  (let [fam (fam (records "@F0@"))]
    (is (= (:partners fam) ["@I0@" "@I2@"]))
    (is (= (:children fam) ["@I3@" "@I4@" "@I5@"]))))
