(ns conference-network.views.pages
  (:require [hiccup.page :refer [html5]]
            [conference-network.views.layout :as layout]))

(defn home [params]
  (layout/common [:h1 "Visualizing conference network"]
                 [:div
                  [:p "This page will try to visualize the communication between
                 conference (or any other event) participants. Communication here will
                 be limited to twitter
                 messages, as they are publicly available. "]
                  [:p "Search for event tweets using event hashtag or/and a handle of the
                  organizer and specify dates on which the event occurred. Due to Twitter API
                  limitations, tweets older than two weeks are not available."]]

                 [:div
                  [:form#get-tweets-form {:action "/get-tweets" :method "POST"}
                   [:p "Event hashtags and/or handles: "]
                   [:input {:type "text" :name "hashtags"}]
                   (if-let [err (:errors (:flash params))]
                     [:p#err "Found errors: "(vals err) "."])
                   [:p "Start and end dates: "]
                   [:input {:type "text" :name "startdate"}]
                   [:input {:type "text" :name "enddate"}]
                   [:p
                    [:input {:type "submit" :value "Get tweets!"} ]]]
                   ]))

(defn about []
  (layout/common [:h1 "About"]))

(defn get-tweets [params]
  (layout/common [:h1 "Request parameters"]
                 [:ul (for [[k v] params] [:li k" : "v])]
                 [:p "hashtag value: " (or (:hashtags (:params params)) "nema")]
                 [:p "startdate value: " (:startdate (:params params))]
                 [:p "enddate value: " (:enddate (:params params))]
                 [:p "form-params??: "(interpose "=" (:form-params params))]
                 ))

(defn not-found []
  (layout/common [:div#lost
                  [:h3 "Ooops! The page you requested probably got lost among the parens.."]
                  [:a {:href "/"} "Try from the beginning..."]]))
