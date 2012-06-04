(ns geni.gedcom.common-test
  (:require [clojure.test :refer :all]
            [gedcom.core :refer [parse-gedcom]]
            [geni.gedcom.common :refer [to-geni]]))

(def records (parse-gedcom (str (System/getProperty "user.dir") "/gedcoms/indi.ged")))

(deftest event-test
  (let [profile (to-geni (records "@I0@"))
        union   (to-geni (records "@I0@"))
        event-data {:date {:year 1994, :day 2, :month 2, :circa false}
                    :location {:place_name "The Library"
                               :country "Westeros"
                               :state "Arkham"
                               :city "Metropolis"}}]
    (testing "Profile events are parsed properly."
      (apply = event-data (map profile [:death :birth :burial :baptism])))
    (testing "Union events are parsed properly."
      (apply = event-data (map union [:marriage :divorce])))))

(deftest name-test
  (let [profile (to-geni (records "@I0@"))]
    (testing "Name is parsed into it's correct portion."
      (is (= {:first_name "Mary"
              :middle_name "Marrison Goosian"
              :last_name "Swanson"
              :suffix "jr"}
             (select-keys profile [:first_name :middle_name :last_name :suffix]))))))

(deftest gender-test
  (testing "SEX is parsed into :gender male or female."
    (is (= "male" (-> "@I2@" records to-geni :gender)))
    (is (= "female" (-> "@I0@" records to-geni :gender)))))

(deftest date-test
  (let [expected {:year 1994 :day 2 :month 2 :circa false}
        get-date (fn [label] (get-in (to-geni (records label)) [:birth :date]))]
    (is (apply = expected
               (map get-date
                    ["@I2@" "@I3@" "@I4@" "@I5@" "@I6@"])))))

(deftest union-test
  (let [union (to-geni (records "@F0@"))]
    (is (= (:partners union) ["@I0@" "@I2@"]))
    (is (= (:children union) ["@I3@" "@I4@" "@I5@"]))))
