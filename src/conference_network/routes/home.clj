(ns conference-network.routes.home
  (:require [compojure.core :refer :all]
            [conference-network.views.pages :as pages]
            [java-time :as time]
            [ring.util.response :as ring]
            [conference-network.models.tweets :as tw]
            [conference-network.routes.validations :as v]
            [conference-network.models.db :as db]))

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
      (home (assoc-in cred-request [:session :user] (:username valid-user)))
      (login-form (assoc-in cred-request [:flash :errors] {:creds "Invalid credentials."})))))

(defn log-out
  "logs user out"
  [request]
  (home (assoc request :session {})))

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
          (home (assoc-in request [:session :user] username))))))

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
  (let [form-input (get-form-params req-params)
        hashtags (:hashtags form-input)
        startdate (:startdate form-input)
        enddate (:enddate form-input)]
    (if-let [errors (v/validate-form-fields form-input)]
      (pages/home (assoc req-params :flash {:errors errors
                                            :inputs {:hashtags hashtags
                                                     :startdate startdate
                                                     :enddate enddate}}))
        (tw/everything-function (get-form-params req-params)))))

(defroutes home-routes
           (GET "/" request (home request))
           (POST "/get-tweets" request (get-tweets request))
           (GET "/login" params (login-form params))
           (POST "/login" cred-request (log-user-in cred-request))
           (GET "/logout" request (log-out request))
           (GET "/account" request (account-form request))
           (POST "/account" request (create-account request))
           )
