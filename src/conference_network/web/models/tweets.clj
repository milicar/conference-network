(ns conference-network.web.models.tweets
  (:use [twitter.oauth]
        [twitter.callbacks]
        [twitter.callbacks.handlers]
        [twitter.api.restful])
  (:require [java-time :as time]
            [conference-network.web.models.graph :as graph]))

; is it any better to have these written in some config file? when it's in text anyway..
(def my-creds (make-oauth-creds "3sZ97SzxOsNLwnZgo23MWWbD2"
                                "vxX5vW2OOv6CecIiwMYFNknfdRXvWqu6R05lYCYNH6ScOh9fz2"
                                "866376916596609024-qaXUgSktTkNEOcIn65fwgx6z9Zyw1fU"
                                "rOQ4845OMayzLVksS3spiCAkt6NFHW1iVX92HADqmrEcd"
                                ))


(defn respect-limit
  "puts thread to sleep if twitter call limit is reached"
  [limit-remaining limit-reset]
  (if (= (Integer/parseInt limit-remaining) 0)
    (let [reset_epoch_time (time/instant (* 1000 (Integer/parseInt limit-reset)))
          reset_local_time (time/local-date-time reset_epoch_time (time/zone-id))
          time_to_wait     (time/time-between reset_local_time (time/local-date-time) :seconds)]
      (Thread/sleep (* 1000 time_to_wait)))))

(defn get-tweets!
  "calls Twitter API and downloads tweets for search criteria with paging;
  if rate limit is reached (limit = 180 calls in 15mins), function waits before recursive call,
  but cannot check the limit for the first call. So, if it is called repeatedly with low counts,
  it will exceed limit..
  so far doesn't check if there are newer tweets (published since the search started);
  returns a collection of statuses"
  ([q-params]
   (let [params (assoc q-params :count 100 :include_entities 1)]
     (get-tweets! params [])))
  ([params statuses]
   (let [new-res      (search-tweets :oauth-creds my-creds :params params)
         new-statuses (:statuses (:body new-res))
         all-statuses (apply merge statuses new-statuses)]
     (if (= 100 (count new-statuses))                       ; if there's possibly more, check for limit & call again
       (let [limit-remaining (:x-rate-limit-remaining (:headers new-res))
             limit-reset     (:x-rate-limit-reset (:headers new-res))
             min_id          (dec (apply min (map :id (:statuses (:body new-res)))))
             max_id          (:max_id (:search_metadata (:body new-res)))
             new-params      (assoc params :max_id min_id)]
         (respect-limit limit-remaining limit-reset)
         (get-tweets! new-params all-statuses))
       all-statuses))))


(defn interpret-twitter-datestring
  "interprets twitter's :created_at date format: \"Mon May 28 13:01:21 +0000 2018\"
  returns (only) date, usable by java-time/local-date"
  [datestring]
  (let [d      (clojure.string/split datestring #" ")
        months {"Jan" "01", "Feb" "02", "Mar" "03", "Apr" "04", "May" "05", "Jun" "06",
                "Jul" "07", "Aug" "08", "Sep" "09", "Oct" "10", "Nov" "11", "Dec" "12"}]
    (str (last d) "-" (get months (second d)) "-" (get d 2))))


(defn filter-by-timeframe
  [statuses start end]
  (filter #(and (apply time/after? (map time/local-date [(interpret-twitter-datestring (:created_at %)) start]))
                (apply time/before? (map time/local-date [(interpret-twitter-datestring (:created_at %)) end]))) statuses))


(defn user-not-in-nodes
  "helps code readability; checks if user is not already in the nodes"
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

; ? REFACTOR next two functions? structure has to be transformed for creating a graph later, but I prefer
; this map structure instead of vectors.
; To be clear, it's {:123 {:456 1, :789 1},...} instead of [[:123 :456 {:weight 1}][:123 :789 {:weight 1}]]
; If function != models.graph/parse-edges is found, then refactor ok
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

; see comment above
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
         (add-user-node user)                               ;refactor at some point later
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
    (if (user res)
      (update res user inc)
      (assoc res user 1))))

(defn number-of-tweets-per-user
  "counts tweets/statuses per user
  input: statuses
  output: map {:user-id statuses-count}"
  [statuses]
  (reduce num-one-status {} statuses))

