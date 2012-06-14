(ns geni.gedcom.web.views.interface
  (:require [noir.core :refer [defpartial defpage]]
            [noir.session :as session]
            [noir.response :refer [content-type]]
            [geni.gedcom.web.models.interface :refer [request-upload request-import]]
            [hiccup.form :refer [label text-area file-upload submit-button drop-down]]
            [hiccup.util :refer [*base-url*]]))

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
    (drop-down "indi" (for [[k v] names] [(get v "name") k]))
    [:br]
    (submit-button "Submit")]])

(defpage [:post "/pick"] {:keys [token gedcom]}
  (session/put! :token token)
  (let [results (request-upload (:tempfile gedcom) token)]
    (if-let [error (get results "error")]
      (content-type "text/plain" error)
      (pick (get results "names")))))

(defpage [:post "/finish"] {:keys [indi]}
  (content-type "text/plain" (request-import indi (session/get :token))))

(defpage "/" []
  (upload))