(ns conference-network.views.pages
  (:require [hiccup.page :refer [html5]]
            [hiccup.form :as f]
            [conference-network.views.layout :as layout]))

(defn home []
  (layout/common [:h1 "Visualizing conference network"]
                 [:div
                  [:p "This page will try to visualize the communication between
                 conference (or any other event) participants. Communication here will
                 be limited to twitter
                 messages, as they are publicly available. "]
                  [:p "Search for event tweets using event hashtag, and specify dates
                  on which the event occurred."]]
                 [:div
                  [:form {:action "/get-tweets" :method "POST"}
                   [:p "Event hashtag: "]
                   [:input {:type "text" :name "hashtag"}]
                   [:p "Start and end dates: "]
                   [:input {:type "text" :name "startdate"}]
                   [:input {:type "text" :name "enddate"}]
                   [:p
                    [:input {:type "submit" :value "Get tweets!"} ]]
                   ]]))

(defn about []
  (layout/common [:h1 "About"]))

(defn not-found []
  (layout/common [:div#lost
                  [:h3 "Ooops! The page you requested probably got lost among the parens.."]
                  [:a {:href "/"} "Try from the beginning..."]]))
