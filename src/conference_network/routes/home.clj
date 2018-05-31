(ns conference-network.routes.home
  (:require [compojure.core :refer :all]
            [conference-network.views.pages :as pages]
            [bouncer.core :as b]
            [bouncer.validators :as v]
            [java-time :as time]))

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

(defn valid-local-date?
  "wraps java-time parsing of the date so as not to throw exception"
  [date-string]
  (time/local-date?
    (try
      (time/local-date "yyyy-MM-dd" (clojure.string/trim date-string))
       (catch Exception e))))

(defn valid-timeframe?
  "for two date-strings checks if first is not after the second"
  [startdate enddate]
  (apply (complement time/after?)
         (map #(time/local-date "yyyy-MM-dd" %) [startdate enddate])))

(defn validate-form-fields
  [req-params]
  (let [form-params (get-form-params req-params)]
    (first
      (b/validate form-params
                  :hashtags [[v/every #(re-matches #"[@|#]\S+" %) :message "Check if all hashtags start with # and handles with @"]]
                  :startdate [[valid-local-date? :message "Check the date format."]]
                  :enddate [[valid-local-date? :message "Check the date format."]
                            [#(valid-timeframe? (:startdate form-params) %) :message "Dates are in the wrong order."]]))))

(defn get-tweets
  "routes further depending on the validity of parameters sent through form\""
  [req-params]
  (if-let [errors (validate-form-fields req-params)]
    (pages/home (assoc req-params :flash {:errors errors}))
    (pages/get-tweets req-params)))

(defroutes home-routes
  (GET "/" request (home request))
  (GET "/about" [] (about))
  (POST "/get-tweets" request (get-tweets request)))
