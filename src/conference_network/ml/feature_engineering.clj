(ns conference-network.ml.feature-engineering
  (:require [conference-network.web.models.tweets :as tweets]
            [conference-network.web.models.graph :as graph]))


(defn get-raw-features
  "collects all feaures in one space for further manipulation
  input: graph, tweets
  output: rows of observations in form of {:id :userID :feature1 f1 ..}"
  [graph tweets]
  (let [graph-features (graph/nodes-graph-metrics graph)
        tweets-features (tweets/number-of-tweets-per-user tweets)]
    (map #(assoc % :num-tweets ((:id %) tweets-features)) graph-features)))

(defn deal-with-nils-and-nans
  "...of outrageous fortune, and, by encoding, end them...
  nils are counts of tweets for nodes that were only mentioned, or replied to,
  but didn't tweet themselves, those should be 0; nils are also results for some
  metrics for nodes that are isolates; NaNs are results for closeness centrality
  for some nodes (but not isolates); those values are encoded as numerical,
  but different cases
  input: rows of observations
  output: the same"
  [raw-featured-observations]
  (let [fix-betweenness-nil (fn [row] (if (nil? (:betweenness row)) (merge row {:betweenness -1.0}) row))
        fix-closeness-nil   (fn [row] (if (nil? (:closeness row)) (merge row {:closeness -1.0}) row))
        fix-pagerank-nil    (fn [row] (if (nil? (:pagerank row)) (merge row {:pagerank -1.0}) row))
        fix-closeness-nan   (fn [row] (if (= "NaN" (str (:closeness row))) (merge row {:closeness -0.5}) row))
        fix-numtweets-nil   (fn [row] (if (nil? (:num-tweets row)) (merge row {:num-tweets 0}) row))]
    (->> raw-featured-observations
         (map fix-betweenness-nil)
         (map fix-closeness-nil)
         (map fix-closeness-nan)
         (map fix-pagerank-nil)
         (map fix-numtweets-nil))))


;(defn rescale-features
;  )



