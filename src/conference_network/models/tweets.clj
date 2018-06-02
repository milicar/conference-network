(ns conference-network.models.tweets
  (:use [twitter.oauth]
        [twitter.callbacks]
        [twitter.callbacks.handlers]
        [twitter.api.restful])
  (:import [twitter.callbacks.protocols SyncSingleCallback])
  (:require [java-time :as time]))

; is it any better to have these written in some config file? when it's in text anyway..
(def my-creds (make-oauth-creds "3sZ97SzxOsNLwnZgo23MWWbD2"
                                "vxX5vW2OOv6CecIiwMYFNknfdRXvWqu6R05lYCYNH6ScOh9fz2"
                                "866376916596609024-qaXUgSktTkNEOcIn65fwgx6z9Zyw1fU"
                                "rOQ4845OMayzLVksS3spiCAkt6NFHW1iVX92HADqmrEcd"
                                ))


(defn wrap-search-tweets
  [params]
  (search-tweets :oauth-creds my-creds :params params))

(def months {"Jan" "01", "Feb" "02", "Mar" "03", "Apr" "04", "May" "05", "Jun" "06",
             "Jul" "07", "Aug" "08", "Sep" "09", "Oct" "10", "Nov" "11", "Dec" "12"})

(defn interpret-twitter-datestring
  "interprets twitter's :created_at date format: \"Mon May 28 13:01:21 +0000 2018\"
  returns (only) date, usable by java-time/local-date"
  [datestring]
  (let [d (clojure.string/split datestring #" ")]
    (str (last d) "-" (get months (second d)) "-" (get d 2)))
  )

(defn filter-by-timeframe
  [statuses start end]
  (filter #(and (apply time/after? (map time/local-date [(interpret-twitter-datestring (:created_at %)) start]))
                (apply time/before? (map time/local-date [(interpret-twitter-datestring (:created_at %)) end]))) statuses))

(defn everything-function
  "first, download tweets, then filter them by timeframe"
  [form-params]
  (let [querystr (:hashtags form-params)
        end    (time/plus (time/local-date (:enddate form-params)) (time/days 1))
        start   (time/minus (time/local-date (:startdate form-params)) (time/days 1))
        response (wrap-search-tweets {:q querystr :until end :count 100 :content_type "recent"})]
    (filter-by-timeframe (get-in response [:body :statuses]) start end))

    )

