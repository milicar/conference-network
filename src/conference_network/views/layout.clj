(ns conference-network.views.layout
  (:require [hiccup.page :refer [html5 include-css]]))

(defn common [& body]
  (html5
    [:head
     [:title "Visualize conference's network"]
     (include-css "/css/screen.css")]
    [:body body]))

