(ns conference-network.test.web.models.tweets
  (:require [conference-network.web.models.tweets :as mtw]
            [java-time :as time])
  (:use [twitter.oauth]
        [twitter.callbacks]
        [twitter.callbacks.handlers]
        [twitter.api.restful]
        [clojure.test]
        [org.senatehouse.expect-call]
        [midje.sweet]
        [clojure.data]))


(deftest check-get-tweets
  (with-expect-call (search-tweets [:oauth-creds _ :params p]
                                   (is (= "#somehash" (:q p)))
                                   (is (= (time/local-date "2018-06-01") (:until p)))
                                   (is (= 100 (:count p)))
                                   (is (= "recent" (:content_type p))))
                    (mtw/get-tweets-and-graph {:hashtags "#somehash" :startdate "2018-05-30" :enddate "2018-05-31"})))

(facts "about interpreting-datestring"
       (fact (mtw/interpret-twitter-datestring "Mon May 28 13:01:21 +0000 2018") => "2018-05-28")
       (fact "Twitter should provide meaningful datestring, I just translate.."
             (mtw/interpret-twitter-datestring "Any May 33 00:00:00 +0000 0000") => "0000-05-33"))


(facts "about adding or updating edges for mentions in tweets"
       (let
         [graph-elements  {:nodes {} :edges {}}
          status_author   {:id_str "123" :id 123}
          status_mention1 {:entities {:user_mentions [{:id 789 :id_str "789"},
                                                      {:id 456 :id_str "456"},
                                                      {:id 123 :id_str "123"}]}}
          status_mention2 {:entities {:user_mentions [{:id 123 :id_str "123"}]}}]

         (mtw/update-edges-mentions status_author status_mention1 graph-elements) =>
         {:nodes {}, :edges {:123 {:789 1, :456 1}}}
         (mtw/update-edges-mentions status_author status_mention2 graph-elements) =>
         {:nodes {}, :edges {}}
         (->> (mtw/update-edges-mentions status_author status_mention1 graph-elements)
              (mtw/update-edges-mentions status_author status_mention1)) =>
         {:nodes {}, :edges {:123 {:789 2, :456 2}}}))


(facts "about adding or updating edges for replies to another user"
       (let
         [graph-elements      {:nodes {} :edges {}}
          status_author       {:id_str "123" :id 123}
          status_is_reply_to0 {}
          status_is_reply_to1 {:in_reply_to_user_id_str "789" :in_reply_to_user_id 789},
          status_is_reply_to2 {:in_reply_to_user_id_str "123" :in_reply_to_user_id 123}]

         (mtw/update-edges-replies status_author status_is_reply_to1 graph-elements) =>
         {:nodes {}, :edges {:123 {:789 1}}}
         (mtw/update-edges-replies status_author status_is_reply_to2 graph-elements) =>
         {:nodes {}, :edges {}}
         (mtw/update-edges-replies status_author status_is_reply_to0 graph-elements) =>
         {:nodes {}, :edges {}}
         (->> (mtw/update-edges-replies status_author status_is_reply_to1 graph-elements)
              (mtw/update-edges-replies status_author status_is_reply_to1)) =>
         {:nodes {}, :edges {:123 {:789 2}}}))


;
;(defn contains-only-dates
;  [actual]
;  (let [dates (map time/local-date "2018-05-24")]
;    (every? true?
;            (map #(contains? dates %)
;                 (map #(time/local-date (mtw/interpret-twitter-datestring (:created_at %))) actual)))))
;
;(facts "about filtering tweets by timeframe, where start and end dates are adjusted by calling function"
;       (let [sample-tweets (get-in (read-string (slurp "sample-search.txt")) [:body :statuses])]
;         (fact "single day timeframe for day x means find all days after (x-1) and before (x+1)"
;               (mtw/filter-by-timeframe sample-tweets "2018-05-23" "2018-05-25")) => contains-only-dates))

;(let [sample-tweets (get-in (read-string (slurp "sample-search.txt")) [:body :statuses])
;      in-timeframe  (mtw/filter-by-timeframe sample-tweets "2018-05-23" "2018-05-25")
;      out-timeframe (diff sample-tweets in-timeframe)]
;  (str "IN: " (apply str (map :created_at in-timeframe)) " OUT: " (apply str (map :created_at out-timeframe) "\n"))
;  out-timeframe)
;
;
;

