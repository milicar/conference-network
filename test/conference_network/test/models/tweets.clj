(ns conference-network.test.models.tweets
  (:require [conference-network.models.tweets :as mtw]
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

