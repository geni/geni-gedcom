(ns geni.gedcom.web.common
  (:require [clojure.java.io :refer [reader resource]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [clojure.tools.logging :refer [error]])
  (:import java.util.Properties))

(defmacro catch-exception-string
  "If an exception occurs while excuting body, print
   it to a string and return the string. Also log the error
   with the s string."
  [s & body]
  `(try ~@body
        (catch Exception e#
          (error e# ~s)
          (with-out-str (print-stack-trace e#)))))

(defn ^:private read-properties []
  (with-open [f (reader (resource "gedcom.properties"))]
    (into {} (doto (Properties.) (.load f)))))

(def config (read-properties))