(ns conference-network.routes.validations
  (:require [java-time :as time]
            [bouncer.core :as b]
            [bouncer.validators :as v]
            [conference-network.models.db :as db]))


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
  "checks if search string is present and dates are in correct format and order"
  [req-params]
  (first
    (b/validate req-params
                :hashtags [[v/required :message "Some search terms must be supplied."]]
                :startdate [[valid-local-date? :message "Check start date format."]]
                :enddate [[valid-local-date? :message "Check end date format."]
                          [#(valid-timeframe? (:startdate req-params) %) :message "Dates are in the wrong order."]])))


(defn validate-login-input
  "checks if username and password fields are empty"
  [credentials]
  (first
    (b/validate credentials
                :username [[v/required :message "Username cannot be empty."]]
                :password [[v/required :message "Password cannot be empty."]])))

(defn validate-user
  "checks if there is a user with specified credentials in the database"
  [user pass]
  (if-let [user (db/check-user user pass)]
    (first user)))

(defn username-reserved?
  [user]
  (not (empty? (db/get-user user))))