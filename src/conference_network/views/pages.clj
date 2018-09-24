(ns conference-network.views.pages
  (:require [hiccup.page :refer [html5]]
            [conference-network.views.layout :as layout]
            [conference-network.models.graph :as graph]
            [conference-network.models.db :as db]
            [noir.session :as session]
            [conference-network.views.vega :as vega]))

(defn home [params]
  (layout/common
    [:h1 "Visualizing conference network"]
    [:div
     [:p "This application will try to visualize the communication between
          conference (or any other event) participants. Communication here will
          be limited to Twitter messages, as they are publicly available. "]
     [:p "Search for event tweets using search terms such as event hashtag, a handle of the
          organizer or any other term that may help narrow the search. Note that using too many search
          terms might result in very few tweets found. Also note that due to Twitter Search API
          limitations, tweets older than one week are not available."]
     [:p "You can specify start and end date of the event. If you don't provide the dates,
          defaults of one week up to now will be used."]]

    (let [err-inputs (:params params)
          err-msg    (:errors (:flash params))]
      [:div#login
       [:form#get-tweets-form {:action "/visualize" :method "POST"}
        [:p "Event hashtags and/or handles: "]
        [:input {:type "text" :name "hashtags"
                 :value (or (:hashtags err-inputs) nil)}]
        [:p "Start and end dates: (format: YYYY-MM-DD)"]
        [:input {:type "text" :name "startdate"
                 :value (or (:startdate err-inputs) "")}]
        [:br]
        [:input {:style "margin-top: 0.5%" :type "text" :name "enddate"
                 :value (or (:enddate err-inputs) "")}]
        [:p
         [:input {:type "submit" :value "Get tweets!"}]]
        (if err-msg
          [:p#err "Errors found: " (for [x (vals err-msg)] [:li x])])
        ]])))

(defn login
  [params]
  (layout/common
    [:h2 "Log into your account:"]
    [:div
     [:form#login {:action "/login" :method "POST"}
      [:p "Username:"]
      [:input {:type "text" :name "username" :value nil}]
      [:p "Password:"]
      [:input {:type "password" :name "password" :value nil}]
      [:p
       [:input {:type "submit" :value "Log in!"}]]
      (if-let [err (:errors (:flash params))]
        [:p#err "Errors found: " (for [x (vals err)] [:li x])])]]))

(defn create-account
  [params]
  (layout/common
    [:h2 "Create account:"]
    [:div
     [:form#login {:action "/account" :method "POST"}
      [:p "Username:"]
      [:input {:type "text" :name "username" :value nil}]
      [:p "Password:"]
      [:input {:type "password" :name "password" :value nil}]
      [:p
       [:input {:type "submit" :value "Create account!"}]]
      (if-let [err (:errors (:flash params))]
        [:p#err "Errors found: " (for [x (vals err)] [:li x])])]]))

(defn my_graphs
  [params]
  (layout/common
    (for [g (:graphs (:flash params))]
      [:tr
       [:td [:form {:action "/show_graph" :method "POST"}
             [:input {:type "hidden" :name "show-graph" :value (:graph g)}]
             [:input {:type "hidden" :name "graph-name" :value (:graph_name g)}]
             [:p (:graph_name g)]
             [:input {:type "submit" :value "Show graph"}]]]
       [:td
        [:form {:action "/delete_graph" :method "POST"}
         [:input {:type "hidden" :name "delete-graph" :value (:graph g)}]
         [:input {:type "submit" :value "Delete graph"}]]]])))

(defn visualize
  [{:keys [params tweets-and-graph] :as all}]
  (layout/viz
    [all]
    [:h1 (if-let [gr-name (:graph-name params)]
           (str gr-name " graph")
           (str "Graph for search terms: " (:hashtags params)))]
    [:div#view]
    [:script (str "vegaEmbed('#view', " (vega/make-vega-spec (:graph tweets-and-graph)) ");")]
    [:form {:action "/predict" :method "POST"}
     [:input {:type "hidden" :name "predict" :value (graph/serialize-graph (:graph tweets-and-graph))}]
     [:input {:type "submit" :value "Predict"}]]
    ; print if no vega:
    ;[:div (with-out-str (ubergraph.core/pprint (:graph params)))]
    ; bug! graph equality: "value objects" so two equal graphs can have != ids
    ; so testing serialized value for existence in db, but sometimes it gets it wrong!
    ;[:div (str all)]
    (when-not (nil? (session/get :user))
      (when (empty? (db/get-graph-by-value (graph/serialize-graph (:graph tweets-and-graph))))
        [:div#unimportant
         [:form#login {:action "/save_graph" :method "POST"}
          [:input {:type "hidden" :name "new-graph" :value (graph/serialize-graph (:graph tweets-and-graph))}]
          [:p#inline "Graph name:" [:input {:type "text" :name "name"}]]
          [:input#savegraphbutton {:type "submit" :value "Save graph"}]]]))))


(defn not-found
  []
  (layout/lost
    [:div#lost
     [:h3 "Ooops! The page you requested probably got lost among the parens.."]
     [:a {:href "/"} "Try from the beginning..."]]))
