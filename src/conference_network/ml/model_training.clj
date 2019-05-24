(ns conference-network.ml.model-training
  (:require [clojure.data.json :as json]
            [conference-network.web.models.tweets :as tweets]
            [conference-network.web.models.graph :as graph]
            [conference-network.ml.feature-engineering :as fe]
            [conference-network.ml.cross-validate :as cv]
            [conference-network.ml.decision-tree :as tree]))


(comment "Tweets to be used for training the model are tweets about a conference (anual SWAN conference),
held in May 21-22 2018. For the 'result' column, which states whether the user participated (tweeted) in
the conference the next year, tweets were downloaded for the same conference held in May 15-16 2019.
Tweets were downloaded using privileged access to historical tweets, with different endpoints,
parameters and especially rate limits for access. Because of this, tweets were downloaded using curl, and
saved as .txt files.")


(def filenames18 '("traintweets/swan18-01.txt" "traintweets/swan18-02.txt" "traintweets/swan18-03.txt"
                  "traintweets/swan18-04.txt" "traintweets/swan18-05.txt"))
(def filenames19 '( "traintweets/swan19-01.txt" "traintweets/swan19-02.txt" "traintweets/swan19-03.txt"
                    "traintweets/swan19-04.txt" "traintweets/swan19-05.txt" "traintweets/swan19-06.txt"))

(defn txt-to-json
  [filename]
  (json/read-str (slurp filename) :key-fn keyword))


(def swan2018 (flatten (merge (map #(:results (txt-to-json %)) filenames18))))
(def swan2019 (flatten (merge (map #(:results (txt-to-json %)) filenames19))))


(comment "By mistake, in search parameters for 2019. tweets, toDate was not set, so tweets need to be
filtered by timeframe. Timeframe used will be a bit looser, taking a day before and after the event as
the limit. Also, 2018. tweets will be filtered by timeframe.")

(def swan2018 (tweets/filter-by-timeframe swan2018
                                          (java-time/local-date-time 2018 05 20 00)
                                          (java-time/local-date-time 2019 05 24 00)))

(def swan2019 (tweets/filter-by-timeframe swan2019
                                          (java-time/local-date-time 2019 05 14 00)
                                          (java-time/local-date-time 2019 05 18 00)))

(comment "To get a sense of how many users will have the class positive, versus negative:")


(let [swanners18 (set (map #(:id_str (:user %)) swan2018))
      swanners19 (set (map #(:id_str (:user %)) swan2019))
      class-positive(clojure.set/intersection swanners18 swanners19)]
  (map count (list swanners18 swanners19 class-positive)))
; 159 165 - these are the users that actually tweeted; only 30 are the same for both years

(comment "Classes will be rather unbalanced.")

(comment "Extracting features for observations: creating graph for graph metrics, adding
tweets counts, encoding nils and NaNs, scaling feature values that are unbounded, rounding
feature values (because each distinct value is checked as a boundary)")

(let [swan2018graph (graph/make-graph (tweets/extract-graph-elements swan2018))]
  (def swandata (-> (fe/get-raw-features swan2018graph swan2018)
                    (fe/deal-with-nils-and-nans))))

(count (fe/filter-out-nils swandata)) ;there are no other, unexpected nils
; here there are 188 users, because some of them were only mentioned or replied to,
; but didn't tweet themselves

(def swandata (->> (fe/rescale-feature :num-tweets swandata)
                   (fe/rescale-feature :betweenness)))

(def swandata (->> (fe/round-feature :in-degree swandata 3)
                   (#(fe/round-feature :out-degree % 3))
                   (#(fe/round-feature :betweenness % 3))
                   (#(fe/round-feature :closeness % 3))
                   (#(fe/round-feature :pagerank % 3))
                   (#(fe/round-feature :num-tweets % 3))))


(comment "A 'results' column has to be added to training and testing data.")

(def result (clojure.set/union (set (map #(keyword (:id_str (:user %))) swan2019))
                               (set (map #(keyword (:in_reply_to_user_id_str %)) swan2019))
                               (set (->> (flatten (map #(:user_mentions (:entities %)) swan2019))
                                         (map #(keyword (:id_str %)))))))
; since mentioned users and those that were replied to, even if they did not tweet, were considered
; participants of the graph for the 2018 data, the same must be done for 2019 data. Maybe treating
; mentioned participants (eg. Microsoft, Cisco..) should be reconsidered.. another time..

(def swandata (map #(assoc % :result (if (contains? result (:id %))
                                       1
                                       0)) swandata))


(comment "The same feature engineering has to be done when classifying new observations.")

(comment "Now we can see class imbalance again:")

(count (filter #(= (:result %) 0) swandata)) ;148
(count (filter #(= (:result %) 1) swandata)) ;40

(comment "The simplest way to deal with this is to undersample the larger class. Here, only random
sampling will be used. More advanced sampling would include informed undersampling and synthetic
oversampling using knn, sampling with data-cleaning techniques such as Tomek links, cluster-based
sampling methods. Another option would be combining sampling methods with boosting algorithms, or
boosting with cost-sensitive weighting.")

(let [zeros (filter #(= (:result %) 0) swandata)
      ones (filter #(= (:result %) 1) swandata)
      percent (double (/ (count ones) (count zeros)))
      undersampled-zeros (:test-data (cv/divide-data zeros percent 2019))] ;divide-data shuffles data
    (def rebalanced-swandata (concat undersampled-zeros ones)))       ;with seed, asked % is assigned to :test


(let [divided-data (cv/divide-data rebalanced-swandata 0.1 2019)]
      (def test-data (:test-data divided-data))
      (def train-val-data (:train-data divided-data)))


(comment "Before training the model, id column should be removed, since it is not a feature")

(def train-userIDs (map :id train-val-data))
(def train-val-swandata (map #(dissoc % :id) train-val-data))


(comment "Fitting the model without choosing any parameters, without cross-validation, and
evaluating it on test data:")

(def tree (tree/build-tree train-val-swandata))
(cv/evaluate tree test-data)
; => {:accuracy 0.75, :error 0.25, :precision 0.5, :recall 1.0, :f1-score 0.6666666666666666}


; evaluating the same tree on more data, by cross-validation
(cv/k-fold-cross-validation (partial tree/build-tree) (vec train-val-swandata) 10 :f1-score)
; 0.669949494949495
(cv/k-fold-cross-validation (partial tree/build-tree) (vec train-val-swandata) 10 :precision)
; 0.7133333333333334


(= tree (tree/prune tree 0.5 tree/gini-impurity))
(= tree (tree/prune tree 0.6 tree/gini-impurity))


;(map
;  (fn [mingain]
;    (cv/k-fold-cross-validation (tree/prune (partial tree/build-tree) mingain tree/gini-impurity) (vec train-val-swandata) 5 :precision))
;  (range 0.2 1.0 0.1))

(map #(tree/prune tree % tree/gini-impurity) (range 0.0 1. 0.1))







(comment "Applying the same feature engineering on live data")


(def raw-featurizer (comp fe/filter-out-nils fe/deal-with-nils-and-nans fe/get-raw-features))
(def rescaler (comp (partial fe/rescale-feature :betweenness) (partial fe/rescale-feature :num-tweets)))
(def rounder (comp #(fe/round-feature :num-tweets % 3) #(fe/round-feature :pagerank % 3)
                   #(fe/round-feature :closeness % 3) #(fe/round-feature :betweenness % 3)
                   #(fe/round-feature :out-degree % 3) #(fe/round-feature :in-degree % 3)))


(defn extract-features
  [graph tweets]
  (rounder (rescaler (raw-featurizer graph tweets))))


(defn live-predict
  "gets graph and tweets from a page (request)
  input: map with graph and tweets {:graph g :tweets tw}
  output: graph with nodes marked with class"
  [{:keys [graph tweets]}]
  (let [data-ready (extract-features graph tweets)
        results (map #(tree/classify tree %) data-ready)
        new-graph graph ;add nodes attributes!
        ]
    (assoc {} :graph new-graph :results results)))

