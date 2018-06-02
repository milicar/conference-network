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

;(spit "spittles.txt" (search-tweets :oauth-creds my-creds :params {:q "npbgd" :count 40}))
;(search-tweets :oauth-creds my-creds :params {:q "%23"})

;(users-show :oauth-creds my-creds :params {:screen-name "AdamJWynne"})

(defn wrap-search-tweets
  [params]
  (search-tweets :oauth-creds my-creds :params params))



(defn everything-function
  [form-params]
  (let [querystr (:hashtags form-params)
        until    (time/plus (time/local-date (:enddate form-params)) (time/days 1))
        before (time/minus (time/local-date (:startdate form-params)) (time/days 1))]

    (wrap-search-tweets {:q querystr :until until :count 100})

    ))


