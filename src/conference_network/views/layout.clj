(ns conference-network.views.layout
  (:require [hiccup.page :refer [html5 include-css]]))

(defn navigation
  []
  [:div#nav
   [:a#navlink {:href "/login"} "Log in"]
   [:a#navlink {:href "/account"} "Create account"]
   [:p#navlink "Hello"]
   [:a#navlink {:href "/logout"} "Log out"]])

(defn common [& body]
  (html5
    [:head
     [:title "Visualize conference's network"]
     (include-css "/css/screen.css")]
    [:body
     (navigation)
     body]))

