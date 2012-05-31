(ns geni.gedcom.common
  (:require [clojure.string :as string]
            [useful.utils :as utils]))

(defmulti to-geni
  "Parse pieces of GEDCOM records into items consumable by the Geni API."
  first)

(defmethod to-geni :default [_] nil)

(def ^{:doc "Convenience function that gets the :data from the first subrecord of a record."}
  get-data
  (comp :data first second))

;; Extract the city, state, and country out of an ADDR
;; structure.
(defmethod to-geni "ADDR" [record]
  (let [addr (-> record second first)
        extract (fn [k] (-> addr (get k) first :data))]
    {:location {:country (extract "CTRY")
                :state   (extract "STAE")
                :city    (extract "CITY")}}))

;; Extract the place structure from PLAC tags.
(defmethod to-geni "PLAC" [record]
  (let [plac (get-data record)]
    {:location {:place_name plac}}))

;; Dates in GEDCOMs are extremely inconsistent so we must be
;; careful about parsing them in order to support as many formats
;; as possible.
(defn ^:private clean-date
  "Remove characters we don't care about from a DATE."
  [date]
  (string/replace
    date
    #"b\.?c\.?|a\.?d\.?|c\.?a?\.|circa|unknown|FROM|TO|BET|BTN|AFT|BEF|AND|about|ab|ABT|CAL|EST|INT|\(.*\)"
    ""))

(defn ^:private date-to-map
  "For simple date structures where each segment is simply
  separated by a single delimiter (like - or /), split on
  that delimiter and then create a map of :month, :day,
  and :year. Sometimes these are in reverse. In thos cases,
  you can pass a third argument (truthy) to reverse the
  ordering."
  [on date & [reverse?]]
  (zipmap (if reverse?
            [:year :month :day]
            [:day :month :year])
          (map #(Integer. %) (string/split date on))))

(defn ^:private also-lower
  "Produce lowercase versions of each list."
  [l]
  (concat l (for [months l] (map #(.toLowerCase %) months))))

(def ^:private months
  "A list of maps of month names in various languages to
  numbers corresponding with the number of that month in
  the year."
  (map zipmap
       (also-lower
         ;; Short English months.
         [["JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"]
          ;; Long English months.
          ["JANUARY" "FEBRUARY" "MARCH" "APRIL" "MAY" "JUNE" "JULY" "AUGUST" "SEPTEMBER"
           "OCTOBER" "NOVEMBER" "DECEMBER"]
          ;; Short Dutch months.
          ["JAN" "FEB" "MRT" "APR" "MEI" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"]
          ;; Short French months.
          ["VEND" "BRUM" "FRIM" "NIVO" "PLUV" "VENT" "GERM" "FLOR" "PRAI" "MESS" "THER"
           "FRUC" "COMP"]
          ;; Short Hebrew months.
          ["TSH" "CSH" "KSL" "TVT" "SHV" "ADR" "ADS" "NSN" "IYR" "SVN" "TMZ" "AAV" "ELL"]
          ;; Long French months.
          ["VENDEMIAIRE" "BRUMAIRE" "FRIMAIRE" "NIVOSE" "FLUVIOSE" "VENTOSE" "GERMINAL"
           "FLOREAL" "PRAIRIAL" "MESSIDOR" "THERMIDOR" "FRUCTIDOR" "JOUR_COMPLEMENTAIRS"]
          ;; Long Hebrew months.
          ["TISHRI" "CHESHVAN" "KISLEV" "TEVET" "SHEVAT" "ADAR" "ADAR_SHENI" "NISAN" "IYAR"
           "SIVAN" "TAMMUZ" "AV" "ELUL"]])
       (repeat (range 1 13))))

(defn ^:private lookup-month
  "Find the number that matches a month string in various
  languages."
  [month]
  (let [month (and month (.toUpperCase month))]
    (some #(% month) months)))

(defn ^:private parse-component
  "Parse the month, year, and day out of a GEDCOM DATE
  structure."
  [acc component]
  (cond (re-find #"\d+" component)
        (assoc acc
               (if (< 2 (count component)) :year :day)
               (Integer. component))
        (re-find #"\W" component) acc
        :else
        (assoc acc :month (lookup-month component))))

(defmethod to-geni "DATE" [record]
  (let [plac (get-data record)
        date (clean-date plac)
        approximate (re-find #"about|ab|ABT|CAL|EST|INT" plac)]
    {:date (merge {:circa (boolean approximate)}
                  (cond (re-find #"\d{1,2}/\d{1,2}/\d{4}" date) (date-to-map #"/" date)
                        (re-find #"\d{4}/\d{1,2}/\d{1,2}" date) (date-to-map #"/" date :reverse)
                        (re-find #"\d{1,2}-\d{1,2}-\d{4}" date) (date-to-map #"-" date)
                        (re-find #"\d{4}-\d{1,2}-\d{1,2}" date) (date-to-map #"-" date :reverse)
                        :else (reduce parse-component {} (string/split date #"\b"))))}))

(defn event [record k]
  {k (reduce utils/adjoin (mapcat #(map to-geni %) (second record)))})