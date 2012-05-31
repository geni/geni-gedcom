(ns geni.gedcom.parser-test
  (:require [clojure.test :refer :all]
            [clj-gedcom.core :refer [parse-gedcom]]
            [geni.gedcom.indi :as indi]))

(def records (parse-gedcom (str (System/getProperty "user.dir") "/gedcoms/indi.ged")))

(deftest event-test
  (let [person (indi/indi (records "@I0@"))
        event-data {:date {:year 1994, :day 2, :month 2, :circa false}
                    :location {:place_name "The Library"
                               :country "Westeros"
                               :state "Arkham"
                               :city "Metropolis"}}]
    (apply = event-data (map person [:death :birth :burial :baptism]))))

(deftest name-test
  (let [person (indi/indi (records "@I0@"))]
    (testing "Name is parsed into it's correct portion."
      (is (= {:first_name "Mary"
              :middle_name "Marrison Goosian"
              :last_name "Swanson"}
             (select-keys person [:first_name :middle_name :last_name]))))))

(deftest date-test
  (let [expected {:year 1994 :day 2 :month 2 :circa false}
        get-date (fn [ged person] (get-in (indi/indi (ged person)) [:birth :date]))]
    (is (apply = expected
               (map (partial get-date records)
                    ["@I2@" "@I3@" "@I4@" "@I5@" "@I6@"])))))