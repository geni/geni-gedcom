(ns geni.gedcom.web.common
  (:require [clojure.java.io :refer [reader resource]])
  (:import (java.io StringWriter PrintWriter)
           java.util.Properties))

(defmacro catch-exception-string
  "If an exception occurs while excuting body, print
   it to a string and return the string."
  [& body]
  `(try ~@body
        (catch Exception e#
          (let [writer# (StringWriter.)]
            (.printStackTrace e# (PrintWriter. writer#))
            (str writer#)))))

(defn ^:private read-properties []
  (with-open [f (reader (resource "gedcom.properties"))]
    (into {} (doto (Properties.) (.load f)))))

(def config (read-properties))