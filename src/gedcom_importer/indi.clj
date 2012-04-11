(ns gedcom-importer.indi
  (:require [clojure.string :as string]
            [useful.utils :as utils]))

(defmulti to-geni first)

(defmethod to-geni "NAME"
  [record]
  (let [name (-> record second first :data)
        [[first-name & middles] [last-name suffix] :as all]
        (split-with
          #(not (.startsWith % "/"))
          (map first (re-seq #"(/[^/]*\*?/\*?|[^* ]+\*?)"
                             (string/replace-first name #"/" " /"))))]
    {:first_name first-name
     :middle_name (when middles (string/join " " middles))
     :last_name (when last-name (last (re-find #"/(.*)/" last-name)))
     :suffix suffix}))

(defmethod to-geni "ADDR" [record]
  (let [addr (-> record second first)
        extract (fn [k] (-> k addr first :data))]
    {:location {:country (extract "CTRY")
                :state   (extract "STAE")
                :city    (extract "CITY")}}))

(defmethod to-geni "PLAC" [record]
  (let [plac (-> record second first :data)]
    {:location {:place plac}}))

(letfn [(clean-date [date]
          (string/replace
            date
            #"b\.?c\.?|a\.?d\.?|c\.?a?\.|circa|unknown|FROM|TO|BET|BTN|AFT|BEF|AND|about|ab|ABT|CAL|EST|INT|\(.*\)"
            ""))

        (date-to-map [on date & [reverse?]]
          (zipmap (if reverse?
                    [:year :month :day]
                    [:day :month :year])
                  (string/split date on)))

        (lookup-month [month] month)

        (parse-component [acc component]
          (cond (re-find #"\d+" component)
                (assoc acc
                       (if (< 2 (count component)) :year :day)
                       (Integer. component))
                (re-find #"\W" component) acc
                :else
                (assoc acc :month (lookup-month component))))]

  (defmethod to-geni "DATE" [record]
    (let [plac (-> record second first :data)
          date (clean-date plac)
          approximate (re-find #"about|ab|ABT|CAL|EST|INT" plac)]
      (cond (some #{\/} date)
            {:date (date-to-map #"/" date)}
            (re-find #"\d{1,2}-\d{1,2}-\d{4}" date)
            {:date (date-to-map #"-" date)}
            (re-find #"\d{4}-\d{1,2}-\d{1,2}" date)
            {:date (date-to-map #"-" date :reverse)}
            :else
            (reduce parse-component
                    {:circa (boolean approximate)}
                    (string/split date #"\b"))))))

(defmethod to-geni "BIRTH" [record]
  ())

(defmethod to-geni :default [_] nil)

(defn indi-to-geni [records]
  (reduce merge (map to-geni records)))
