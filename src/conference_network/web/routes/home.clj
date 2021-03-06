(ns conference-network.web.routes.home
  (:require [compojure.core :refer :all]
            [conference-network.web.views.pages :as pages]
            [java-time :as time]
            [conference-network.web.models.tweets :as tw]
            [conference-network.web.routes.validations :as v]
            [conference-network.db.db :as db]
            [conference-network.web.models.graph :as g]
            [conference-network.ml.model-training :as model]
            [noir.session :as session]
            [noir.response :as resp]))

(defn home [req-params]
  (pages/home req-params))

(defn login-form
  "shows login form"
  [params]
  (pages/login params))

(defn log-user-in
  "tries to authenticate user and open session"
  [cred-request]
  (if-let [err (v/validate-login-input (:params cred-request))]
    (login-form (assoc-in cred-request [:flash :errors] err))
    (if-let [valid-user (v/validate-user (:username (:params cred-request)) (:password (:params cred-request)))]
      (do (session/put! :user (:username valid-user))
          (resp/redirect "/"))
      (login-form (assoc-in cred-request [:flash :errors] {:creds "Invalid credentials."})))))

(defn log-out
  "logs user out"
  [request]
  (do (session/clear!)
      (resp/redirect "/")))

(defn account-form
  "shows form for creating new user account"
  [request]
  (pages/create-account request))

(defn create-account
  "creates new account for a unique username"
  [request]
  (let [username (:username (:params request))
        password (:password (:params request))]
    (if (v/username-reserved? username)
      (pages/create-account (assoc-in request [:flash :error]
                                      {:err "User with this username already exists."}))
      (do (db/create-account username password)
          (session/put! :user username)
          (resp/redirect "/")))))

(defn my-graphs
  "lists all graphs for a user"
  [request]
  (pages/my_graphs (assoc-in request [:flash :graphs]
                             (db/get-graphs (session/get :user)))))

(defn save-graph
  "saves user's graph to database"
  [request]
  (db/save-graph (session/get :user) (get (:form-params request) "new-graph") (get (:form-params request) "name"))
  (my-graphs request))

(defn show-graph
  "shows a single graph"
  [request]
  (pages/visualize
    (-> (get-in request [:params :show-graph])
        (g/deserialize-graph)
        (#(assoc-in request [:tweets-and-graph :graph] %)))))

(defn delete-graph
  "deletes a graph"
  [request]
  (-> (get-in request [:params :delete-graph])
      (db/get-graph-id)
      (db/delete-graph))
  (my-graphs request))

(defn predict
  "predicts which nodes will not return next year"
  [{:keys [params] :as request}]
  (let [params (read-string (:predict params))
        graph  (g/deserialize-graph (:graph params))
        tweets (:tweets params)
        graph-and-tweets (assoc {} :graph graph :tweets tweets)
        name (:name params)]
    (pages/visualize (-> (assoc request :res-graph (model/live-predict graph-and-tweets)
                                        :name name
                                        :tweets-and-graph {:graph graph})
                         (dissoc :params)
                         ; viz page checks whether the graph from params is already in db and renders the save button or not
                         ; that's why graph is sent again, but tweets are not!
                         ))))

(defn- get-form-params
  "supplies default dates if not entered in form;
  defaults are :enddate now, :startdate 7 days ago"
  [{p :params}]
  (let [enddate   (or (not-empty (:enddate p)) (time/format (time/local-date)))
        startdate (or (not-empty (:startdate p)) (time/format (time/minus (time/local-date enddate) (time/days 7))))]
    (assoc p :startdate startdate :enddate enddate)))

(defn get-tweets
  "routes further depending on the validity of parameters sent through form"
  [req-params]
  (let [form-input (get-form-params req-params)]
    (if-let [errors (v/validate-form-fields form-input)]
      (pages/home (assoc req-params :flash {:errors errors}))
      (pages/visualize (assoc req-params
                         :tweets-and-graph (tw/get-tweets-and-graph (get-form-params req-params)))))))

(defroutes home-routes
           (GET "/" request (home request))
           (POST "/visualize" request (get-tweets request))
           (GET "/login" params (login-form params))
           (POST "/login" cred-request (log-user-in cred-request))
           (GET "/logout" request (log-out request))
           (GET "/account" request (account-form request))
           (POST "/account" request (create-account request))
           (GET "/my_graphs" request (my-graphs request))
           (POST "/save_graph" request (save-graph request))
           (POST "/show_graph" request (show-graph request))
           (POST "/delete_graph" request (delete-graph request))
           (POST "/predict" request (predict request)))
