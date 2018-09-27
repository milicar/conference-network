(ns conference-network.web.views.layout
  (:require [hiccup.page :refer [html5 include-css]]
            [noir.session :as session]))

(defn navigation
  []
  (if (nil? (session/get :user))
    [:div#nav
     [:a#navlink {:href "/"} "Home"]
     [:a#navlink {:href "/login"} "Log in"]
     [:a#navlink {:href "/account"} "Create account"]]
    [:div#nav
     [:a#navlink (str "Hello, " (session/get :user))]
     [:a#navlink {:href "/"} "Home"]
     [:a#navlink {:href "/my_graphs"} "My graphs"]
     [:a#navlink {:href "/logout"} "Log out"]]
    )
  )

(defn common [& body]
  (html5
    [:head
     [:title "Visualize conference's network"]
     (include-css "/css/screen.css")]
    [:body
     (navigation)
     body]))

(defn viz [[req-params] & body]
  (html5
    [:head
     (include-css "/css/screen.css")
     [:script {:src "https://vega.github.io/vega/vega.min.js"}]
     [:script {:src "https://cdn.jsdelivr.net/npm/vega@4.0.0-rc.2"}]
     [:script {:src "https://cdn.jsdelivr.net/npm/vega-lite@2.4.3"}]
     [:script {:src "https://cdn.jsdelivr.net/npm/vega-embed@3.13.2"}]
     ]
    [:body
     (navigation)
     body]))

(defn lost [& body]
  (html5
    [:head
     [:title "Not Found"]
     (include-css "/css/screen.css")]
    [:body body]))