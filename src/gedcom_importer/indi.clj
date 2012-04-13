(ns gedcom-importer.indi
  (:require [clojure.string :as string]
            [useful.utils :as utils]))

(defmulti to-geni
  "Parse pieces of GEDCOM records."
  first)

;; Parse the name of an individual into first_name, middle_name
;; last_name, and suffix parts. The first name is always the very
;; first part of a name. The middle name is everything after the
;; first name up to the first slash (/) that delimits the last
;; name. After the next / is the suffix.
(defmethod to-geni "NAME"
  [record]
  (let [name (-> record second first :data)
        [[first-name & middles] [last-name suffix]]
        (split-with
          #(not (.startsWith % "/"))
          (map first (re-seq #"(/[^/]*\*?/\*?|[^* ]+\*?)"
                             (string/replace-first name #"/" " /"))))]
    {:first_name first-name
     :middle_name (when middles (string/join " " middles))
     :last_name (when last-name (last (re-find #"/(.*)/" last-name)))
     :suffix suffix}))

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
  (let [plac (-> record second first :data)]
    {:location {:place plac}}))

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
          (string/split date on)))

(def ^:private months
  "A list of maps of month names in various languages to
  numbers corresponding with the number of that month in
  the year." 
  (map zipmap
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
         "SIVAN" "TAMMUZ" "AV" "ELUL"]]
       (repeat (range 1 13))))

(defn ^:private lookup-month
  "Find the number that matches a month string in various
  languages."
  [month]
  (some #(% month) months))

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
  (let [plac (-> record second first :data)
        date (clean-date plac)
        approximate (re-find #"about|ab|ABT|CAL|EST|INT" plac)]
    {:date (cond (some #{\/} date) (date-to-map #"/" date)
                 (re-find #"\d{1,2}-\d{1,2}-\d{4}" date) (date-to-map #"-" date)
                 (re-find #"\d{4}-\d{1,2}-\d{1,2}" date) (date-to-map #"-" date :reverse)
                 :else (reduce parse-component
                               {:circa (boolean approximate)}
                               (string/split date #"\b")))}))

;; BIRT, DEAT, and BAPM all have the same general structure.
;; The difference between them is what key we put the results
;; under for passing to the API. Therefore, these methods are
;; very simple.
(letfn [(event [record k]
          {k (reduce utils/adjoin (mapcat #(map to-geni %) (second record)))})]

  (defmethod to-geni "BIRT" [record] (event record :birth))
  (defmethod to-geni "DEAT" [record] (event record :death))
  (defmethod to-geni "BAPM" [record] (event record :baptism)))

;; Handle nils and tags we don't need to support.
(defmethod to-geni :default [_] nil)

(defn indi-to-geni
  "Parse an INDI record and return a map suitable for passing
  to the Geni API."
  [records]
  (reduce merge (map to-geni records)))
