(ns conference-network.models.tweets
  (:use [twitter.oauth]
        [twitter.callbacks]
        [twitter.callbacks.handlers]
        [twitter.api.restful])
  (:require [java-time :as time]
            [conference-network.models.graph :as graph]))

; is it any better to have these written in some config file? when it's in text anyway..
(def my-creds (make-oauth-creds "3sZ97SzxOsNLwnZgo23MWWbD2"
                                "vxX5vW2OOv6CecIiwMYFNknfdRXvWqu6R05lYCYNH6ScOh9fz2"
                                "866376916596609024-qaXUgSktTkNEOcIn65fwgx6z9Zyw1fU"
                                "rOQ4845OMayzLVksS3spiCAkt6NFHW1iVX92HADqmrEcd"
                                ))


(defn get-tweets!
  "calls Twitter API and downloads tweets for search criteria with paging
  so far doesn't handle the case when rate limit is reached (limit = 180 calls in 15mins)
  so far doesn't check if there are newer tweets (published since the search started)
  returns a collection of statuses"
  ([q-params]
    (let [params (assoc q-params :count 100)
          result (search-tweets :oauth-creds my-creds :params params)]
      (if (= 100 (count (:statuses (:body result))))
        (get-tweets! params result (:statuses (:body result)))
        (:statuses (:body result)))))
  ([params result statuses]
   (let [limit-remaining (:x-rate-limit-remaining (:headers result))
         limit-reset     (:x-rate-limit-reset (:headers result))
         min_id          (dec (apply min (map :id (:statuses (:body result)))))
         max_id          (:max_id (:search_metadata (:body result)))
         new-params      (assoc params :max_id min_id)]
     (if (> (java.lang.Integer/parseInt limit-remaining) 0)
       (let [new-res      (search-tweets :oauth-creds my-creds :params new-params)
             new-statuses (apply merge statuses (:statuses (:body new-res)))]
         (if (= 100 (count (:statuses (:body new-res))))
           (get-tweets! params new-res new-statuses)
           new-statuses))))))

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


(defn extract-graph-elements
  "parses statuses one at a time and collects elements for a graph
  input: a collection of statuses from twitter
  output: elements of a graph: {:nodes {:tw-id {:name \"name\" :screen-name \"scr-n\"}}
                               {:edges {:id-src {:id-dest weight/count}}}"
  [all-statuses]
  (let [graph-elements {:nodes {} :edges {}}]
    (reduce update-graph-elements graph-elements all-statuses)))


(defn get-tweets-and-graph
  "downloads tweets and makes a graph; puts both tweets and a graph to a map and returns it
  input: query parameters: {:hashtags #somehash :startdate date :enddate date}
  output: map {:tweets all-the-tweets :graph resulting-graph"
  [form-params]
  (let [querystr (:hashtags form-params)
        end      (time/plus (time/local-date (:enddate form-params)) (time/days 1))
        start    (time/minus (time/local-date (:startdate form-params)) (time/days 1))
        response (get-tweets! {:q querystr :until end :content_type "recent"})]
    (assoc {} :tweets response :graph (-> response
                                              (filter-by-timeframe start end)
                                              (extract-graph-elements)
                                              (graph/make-graph)))))

(defn num-one-status
  "updates tweets count based on one status
  input: counts map: {:user-id count} or empty and one status/tweet
  output: map: {:user-id count}"
  [res status]
  (let [user (keyword (:id_str (:user status)))]
    (if (nil? (user res))
     (assoc res user 1)
     (update res user inc))))

(defn number-of-tweets-per-user
  "counts tweets/statuses per user
  input: statuses
  output: map {:user-id statuses-count}"
  [statuses]
  (reduce num-one-status {} statuses))

