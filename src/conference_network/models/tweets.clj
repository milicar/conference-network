(ns conference-network.models.tweets
  (:use [twitter.oauth]
        [twitter.callbacks]
        [twitter.callbacks.handlers]
        [twitter.api.restful])
  (:import [twitter.callbacks.protocols SyncSingleCallback])
  (:require [java-time :as time]
            [conference-network.models.graph :as graph]))

; is it any better to have these written in some config file? when it's in text anyway..
(def my-creds (make-oauth-creds "3sZ97SzxOsNLwnZgo23MWWbD2"
                                "vxX5vW2OOv6CecIiwMYFNknfdRXvWqu6R05lYCYNH6ScOh9fz2"
                                "866376916596609024-qaXUgSktTkNEOcIn65fwgx6z9Zyw1fU"
                                "rOQ4845OMayzLVksS3spiCAkt6NFHW1iVX92HADqmrEcd"
                                ))


(defn wrap-search-tweets
  [params]
  (search-tweets :oauth-creds my-creds :params params))

(defn interpret-twitter-datestring
  "interprets twitter's :created_at date format: \"Mon May 28 13:01:21 +0000 2018\"
  returns (only) date, usable by java-time/local-date"
  [datestring]
  (let [d (clojure.string/split datestring #" ")
        months {"Jan" "01", "Feb" "02", "Mar" "03", "Apr" "04", "May" "05", "Jun" "06",
                "Jul" "07", "Aug" "08", "Sep" "09", "Oct" "10", "Nov" "11", "Dec" "12"}]
    (str (last d) "-" (get months (second d)) "-" (get d 2))))


(defn filter-by-timeframe
  [statuses start end]
  (filter #(and (apply time/after? (map time/local-date [(interpret-twitter-datestring (:created_at %)) start]))
                (apply time/before? (map time/local-date [(interpret-twitter-datestring (:created_at %)) end]))) statuses))


(defn user-not-in-nodes
  "helps readability; checks if user is not already in the nodes"
  [user-key nodes]
  (nil? (user-key nodes)))

(defn add-user-node
  "adds a user node to graph elements if it's not there already
  returns graph-elements"
  [user graph-elements]
  (let [user-key (keyword (:id_str user))]
    (if (user-not-in-nodes user-key (:nodes graph-elements))
      (assoc-in graph-elements [:nodes user-key]
                (-> {}
                    (assoc :name (:name user))
                    (assoc :screen-name (:screen_name user))))
      graph-elements)))

(defn add-replied-to-node
  "adds nodes for replied-to users; these have only screen names in api response"
  [status graph-elements]
  (if-let [user-key (keyword (:in_reply_to_user_id_str status))]
    (if (user-not-in-nodes user-key (:nodes graph-elements))
      (assoc-in graph-elements [:nodes user-key]
                (-> {}
                    (assoc :screen-name (:in_reply_to_screen_name status))))
      graph-elements)
    graph-elements))

(defn add-mentioned-nodes
  "adds mentioned nodes; calls add-user-node for each mentioned user"
  [status graph-elements]
  (if-let [mentions (:user_mentions (:entities status))]
    (reduce #(add-user-node %2 %1) graph-elements mentions)
    graph-elements))


(defn update-edges-mentions
  "if there are mentions in status, for every mention add or update edge
  *this does not apply to users mentioning themselves, filter those first*
  edge is represented as {:fromID {:toID count}}
  return graph-elements structure"
  [user status graph-elements]
  (let [user-key            (keyword (:id_str user))
        unfiltered-mentions (:user_mentions (:entities status))
        mentions            (filter #(not= (:id user) (:id %)) unfiltered-mentions)]
    (if (not (empty? mentions))
      (assoc-in graph-elements [:edges user-key]
                (merge-with + (user-key (:edges graph-elements))
                            (reduce
                              #(assoc %1 (keyword (:id_str %2)) 1) {} mentions)))
      graph-elements)))

(defn update-edges-replies
  "if a status is in reply to another user (but not a retweet of user's own tweet),
  add or update edge between two users
  edge is represented as {:fromID {:toID count}}
  return graph-elements structure"
  [user status graph-elements]
  (if (and (some? (:in_reply_to_user_id_str status))
           (not= (:id user) (:in_reply_to_user_id status)))
    (let [user-key (keyword (:id_str user))]
      (assoc-in graph-elements [:edges user-key]
                (merge-with + (user-key (:edges graph-elements))
                            (assoc {} (keyword (:in_reply_to_user_id_str status)) 1))))
    graph-elements))

(defn update-graph-elements
  "for one status/tweet, adds author to nodes if it is not there already,
  and adds edges elements if the status mentions other users or if the tweet is a
  retweet of another user's tweet; returns graph-elements structure"
  [graph-elements status]
  (let [user (:user status)]
    (->> graph-elements
      (add-user-node user)         ;refactor at some point later
         (add-replied-to-node status)
         (add-mentioned-nodes status)
         (update-edges-mentions user status)
         (update-edges-replies user status))))


(defn parse-statuses
  "parses statuses one at a time"
  [all-statuses]
  (let [graph-elements {:nodes {} :edges {}}]
    (reduce update-graph-elements graph-elements all-statuses)))


(defn everything-function
  "downloads tweets, then filters them by timeframe, then extracts graph structure from statuses, then makes graph"
  [form-params]
  (let [querystr (:hashtags form-params)
        end      (time/plus (time/local-date (:enddate form-params)) (time/days 1))
        start    (time/minus (time/local-date (:startdate form-params)) (time/days 1))
        response (wrap-search-tweets {:q querystr :until end :count 100 :content_type "recent"})]
    (-> response
        :body
        :statuses
        (filter-by-timeframe start end)
        parse-statuses
        graph/make-graph)
    ))

