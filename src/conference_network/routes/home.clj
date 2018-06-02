(ns conference-network.routes.home
  (:require [compojure.core :refer :all]
            [conference-network.views.pages :as pages]
            [bouncer.core :as b]
            [bouncer.validators :as v]
            [java-time :as time]
            [clojure.core.async :as a]
            [ring.util.response :as ring]
            [conference-network.models.tweets :as tw]))

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

(defn future-function
  [nja]
  (Thread/sleep 10000)
  (ring/redirect "pages/not-found"))


(defn future-try
  []
  (do
   (future (future-function "mamamama"))
      (pages/not-found)))

(defn get-tweets
  "routes further depending on the validity of parameters sent through form"
  [req-params]
  (if-let [errors (validate-form-fields req-params)]
    (pages/home (assoc req-params :flash {:errors errors}))
    ; something along the lines of: call a function that downloads tweets and makes a graph and prepares visualization
    ; in the meantime, route to a page that says "wait for the result", and then, when result is ready, load the
    ; visualization and buttons and all (to that same page).. so, a separate thread for computations?

    ;(do (let [t (a/thread (Thread/sleep 10000) "pages/not-found")]
    ;      (a/go  (ring/redirect (a/<! t))))
    ;    (pages/get-tweets req-params))))

    ;(do (let [f (future (future-function "anystring"))]
    ;      (pages/get-tweets req-params)
    ;      (if (future-done? f) @f)))))

    ;(let [p (promise)]
    ;  (future (Thread/sleep 5000) (ring/redirect @p))
    ;  (deliver p "pages/not-found")
    ;  (pages/get-tweets req-params))))

    ;    so, printlns work, but redirects don't... going with one thread for now..
    (tw/everything-function (get-form-params req-params))))

(defroutes home-routes
  (GET "/" request (home request))
  (GET "/about" [] (about))
  (POST "/get-tweets" request (get-tweets request)))
