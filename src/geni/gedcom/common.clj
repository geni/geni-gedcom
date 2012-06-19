(ns geni.gedcom.common
  (:require [clojure.string :as string]
            [useful.fn :refer [to-fix fix]]
            [useful.utils :refer [adjoin]]))

(defmulti to-geni
  "Parse pieces of GEDCOM records into items consumable by the Geni API."
  (to-fix vector? first
          map?    :tag
          nil))

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
  (string/trim
   (string/replace
    date
    #"b\.?c\.?|a\.?d\.?|c\.?a?\.|circa|unknown|antes\s+de|FROM|TO|BET|BTN|AFT|BEF|AND|about|ab|ABT|CAL|EST|INT|\(.*\)"
    "")))

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
        (let [[_ month num] (re-find #"([a-zA-Z]*)(\d+)" component)
              date {(if (< 2 (count num)) :year :day) (Integer. num)}]
          (if (seq month)
            (merge acc (assoc date :month (lookup-month month)))
            (merge acc date)))
        (re-find #"\W" component) acc
        :else
        (assoc acc :month (lookup-month component))))

(defmethod to-geni "DATE" [record]
  (when-let [plac (get-data record)]
    (let [date (clean-date plac)
          approximate (re-find #"about|ab|ABT|CAL|EST|INT|antes\s+de" plac)]
      {:date (merge {:circa (boolean approximate)}
                    (cond (re-find #"\d{1,2}/\d{1,2}/\d{4}" date) (date-to-map #"/" date)
                          (re-find #"\d{4}/\d{1,2}/\d{1,2}" date) (date-to-map #"/" date :reverse)
                          (re-find #"\d{1,2}-\d{1,2}-\d{4}" date) (date-to-map #"-" date)
                          (re-find #"\d{4}-\d{1,2}-\d{1,2}" date) (date-to-map #"-" date :reverse)
                          :else (reduce parse-component {} (string/split date #"\b"))))})))

(defn event [record k]
  (when-let [value (reduce adjoin (mapcat #(map to-geni %) (second record)))]
    {k value}))

;; Profile methods

;; Parse the name of an individual into first_name, middle_name
;; last_name, and suffix parts. The first name is always the very
;; first part of a name. The middle name is everything after the
;; first name up to the first slash (/) that delimits the last
;; name. After the next / is the suffix.
(defmethod to-geni "NAME"
  [record]
  (when-let [name (get-data record)]
    (let [[[first-name & middles] [last-name suffix]]
          (split-with
           #(not (.startsWith % "/"))
           (map first (re-seq #"(/[^/]*\*?/\*?|[^* ]+\*?)"
                              (string/replace-first name #"/" " /"))))]
      {:first_name first-name
       :middle_name (when middles (string/join " " middles))
       :last_name (when last-name (last (re-find #"/(.*)/" last-name)))
       :suffix suffix})))

;; BIRT, DEAT, BURI, and BAPM all have the same general structure.
;; The difference between them is what key we put the results
;; under for passing to the API. Therefore, these methods are
;; very simple.
(defmethod to-geni "BIRT" [record] (event record :birth))
(defmethod to-geni "DEAT" [record] (assoc (event record :death) :is_alive false))
(defmethod to-geni "BAPM" [record] (event record :baptism))
(defmethod to-geni "BURI" [record] (event record :burial))

(defmethod to-geni "FAMS" [record]
  {:partner (map :data (second record))})

(defmethod to-geni "FAMC" [record]
  {:child (map :data (second record))})

(defmethod to-geni "SEX" [record]
  {:gender (case (get-data record)
             "M" "male"
             "F" "female"
             nil)})

(defmethod to-geni "INDI" [record]
  (let [profile (reduce adjoin (map to-geni record))]
    (assoc (if (contains? profile :is_alive)
             profile
             (assoc profile :is_alive true))
      :record-type :indi)))

(defn union-ids
  "Return the union ids linked to from this profile."
  [profile]
  (mapcat profile [:child :partner]))

;; Union methods

;; The CHIL tag is the only one that there should ever be
;; more than one of inside of a FAM record.
(defmethod to-geni "CHIL" [record]
  {:children (map :data (second record))})

(defmethod to-geni "HUSB" [record]
  {:partners [(get-data record)]})

(defmethod to-geni "WIFE" [record]
  {:partners [(get-data record)]})

(defmethod to-geni "MARR" [record] (event record :marriage))
(defmethod to-geni "DIV" [record] (event record :divorce))

(defmethod to-geni "FAM" [record]
  (assoc (reduce adjoin {} (map to-geni record))
    :record-type :fam))

(defn profile-ids
  "Return the profile ids linked to from this union."
  [union]
  (mapcat union [:children :partners]))
