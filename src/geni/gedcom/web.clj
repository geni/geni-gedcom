(ns geni.gedcom.web
  (:require [noir.core :refer [defpage defpartial]]
            [noir.server :refer [start gen-handler]]
            [noir.response :refer [json]]
            [noir.request :refer [ring-request]]
            [noir.session :as session]
            [hiccup.form :refer [label text-area file-upload submit-button drop-down]]
            [hiccup.util :refer [*base-url*]]
            [geni.gedcom.import :refer [import-gedcom]]
            [gedcom.core :refer [parse-gedcom]]
            [geni.gedcom.common :refer [to-geni]]
            [geni.core :as geni]
            [clojure.java.io :refer [resource reader]]
            [clojure.string :refer [join]]
            [clj-http.client :as http]
            [cheshire.core :refer [parse-string]])
  (:import java.util.Properties
           (java.io PrintWriter StringWriter))
  (:gen-class))

(defn read-properties []
  (with-open [f (reader (resource "gedcom.properties"))]
    (into {} (doto (Properties.) (.load f)))))

(def config (read-properties))

(defpartial upload []
  [:body
   [:form {:method "POST" :action (str *base-url* "/pick") :enctype "multipart/form-data"}
    (label "token-label" "Geni API Token: ")
    (text-area "token")
    [:br]
    (label "gedcom-label" "GEDCOM File: ")
    (file-upload "gedcom")
    [:br]
    (submit-button "Submit")]])

(defpartial pick [names]
  [:body
   [:form {:method "POST" :action (str *base-url* "/finish")}
    (label "dropdown-label" "Start with: ")
    (drop-down "drop" (for [[k v] names] [(get v "name") k]))
    [:br]
    (submit-button "Submit")]])

(defmacro catch-exception-string
  "If an exception occurs while excuting body, print
   it to a string and return the string."
  [& body]
  `(try ~@body
        (catch Exception e#
          (let [writer# (StringWriter.)]
            (.printStackTrace e# (PrintWriter. writer#))
            (str writer#)))))

(def cache (atom {:id 0}))

(defn cache-records
  "Cache records that may be imported in the near future."
  [cache records token]
  (let [id (inc (:id cache))]
    (assoc cache
      token {:id id, :records records}
      :id id)))

(defn remove-records
  "If the cached id is the same as id, remove the records
   associated with token."
  [cache token id]
  (if (= id (get-in cache [token :id]))
    (dissoc cache token)
    cache))

(defn future-clean
  "Setup a thread that waits for 5 minutes and then removes
   cached records under token if the associated id is the same
   as id."
  [token id]
  (future
    (Thread/sleep 300000)
    (swap! cache remove-records token id)))

(defn names
  "Takes gedcom records and returns a map of names from INDI
   records to their corresponding ids."
  [records]
  (reduce (fn [acc [k v]]
            (if (= :indi (:record-type v))
              (assoc acc
                k {:name (join " " (filter identity
                                           ((juxt :first_name
                                                  :middle_name
                                                  :last_name)
                                            v)))})
              acc))
          {}
          records))

(defpage [:post "/upload"] {:keys [gedcom token]}
  (json
   (let [results (catch-exception-string
                  (parse-gedcom (:tempfile gedcom) to-geni))]
     (if (map? results)
       (let [id (:id (swap! cache cache-records results token))]
         (future-clean token id)
         {:names (names results)})
       {:error results}))))

;; Need to ask Scott to add count keys to import_tree's results.
(defn count-profiles
  "Count the number of profiles returned by the Geni API."
  [results]
  (count (filter #(.startsWith (val %) "profile") results)))

(defn import-records
  "Call import-gedcom respecting settings in gedcom.properties."
  [records id token]
  (binding [geni/*base* (config "url")
            geni/*insecure* (Boolean/valueOf (config "insecure"))]
    (catch-exception-string (import-gedcom records id token))))

(defpage [:post "/import"] {:keys [id token]}
  (json
   (if-let [records (:records (get @cache token))]
     (let [results (import-records records id token)]
       (swap! cache dissoc token)
       (if (map? results)
         {:profiles_imported (count-profiles results)}
         {:error results}))
     {:error "No records associated with this token."})))

(defn create-url [to]
  (let [request (ring-request)]
    (str (name (:scheme request)) "://"
         (:server-name request)
         (when-let [port (:server-port request)]
           (str ":" port))
         (config "base.url")
         to)))

(defpage [:post "/pick"] {:keys [token gedcom]}
  (session/put! :token token)
  (let [results (parse-string
                 (:body
                  (http/post (create-url "/upload")
                             {:multipart {:gedcom (:tempfile gedcom)
                                          :token token}})))]
    (if-let [error (:error results)]
      (:message error)
      (pick (get results "names")))))

(defpage [:post "/finish"] {:keys [drop]}
  (:body
   (http/post (create-url "/import")
              {:form-params {:id drop
                             :token (session/get :token)}})))

(defpage "/" []
  (upload))

(def handler (gen-handler {:mode :prod
                           :base-url (config "base.url")}))

(defn -main []
  (start 8080 {:mode :prod
               :base-url (config "base.url")}))

;; war test code
(comment
  (import org.mortbay.jetty.Server
          org.mortbay.jetty.webapp.WebAppContext)
  
  (doto (Server. 8080)
    (.setHandler (WebAppContext. "target/importer.war" "/"))
    .start))