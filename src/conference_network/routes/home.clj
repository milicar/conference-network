(ns conference-network.routes.home
  (:require [compojure.core :refer :all]
            [conference-network.views.pages :as pages]
            [bouncer.core :as b]
            [bouncer.validators :as v]))

(defn home [req-params]
  (pages/home req-params))

(defn about []
  (pages/about))

(defn get-form-params
  "takes request parameters and returns only form parameters with string of hashtags restructured
  into collection of separate words"
  [{p :params}]
  (let [strs (clojure.string/split (:hashtags p) #" ")]
    (assoc p :hashtags strs)))

(defn validate-form-fields
  [req-params]
  (let [form-params (get-form-params req-params)]
    (first
      (b/validate form-params
                  :hashtags [[v/every #(re-matches #"[@|#]\S+" %)]]))))

(defn get-tweets
  "routes"
  [req-params]
  (if-let [errors (validate-form-fields req-params)]
    (pages/home (assoc req-params :flash {:errors errors}))
    (pages/get-tweets req-params)))

(defroutes home-routes
  (GET "/" request (home request))
  (GET "/about" [] (about))
  (POST "/get-tweets" request (get-tweets request)))
