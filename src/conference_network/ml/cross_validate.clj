(ns conference-network.ml.cross-validate
  (:require [conference-network.ml.decision-tree :as dtree]))


(defn deterministic-shuffle
  "Return a random permutation of coll"
  [^java.util.Collection coll seed]
  (let [array-list     (java.util.ArrayList. coll)
        rand-generator (java.util.Random. seed)]
    (java.util.Collections/shuffle array-list rand-generator)
    (clojure.lang.RT/vector (.toArray array-list))))


(defn divide-data
  "takes random samples for test and validation datasets; uses deterministic shuffle,
  for reproducibility (if seed is not provided, shuffle is randomised by random seed);
  after shuffle, saves the order of data
  !!throws exception if there are no data in either subset!!
  input: observations in coll or seq
  output: {:train-data [observations] :test-data [observations]}"
  ([data ratio]
   (let [seed (* 1000 (rand))]
     (divide-data data ratio seed)))
  ([data ratio seed]
   (let [shuffled-data (deterministic-shuffle data seed)
         n             (long (* ratio (count data)))
         test-data     (subvec shuffled-data 0 n)
         train-data    (subvec shuffled-data n)]
     (if (and (< 0 (count test-data)) (< 0 (count train-data)))
       {:test-data test-data :train-data train-data}
       (throw (Exception. "No data in test or train subset!"))))))


(defn binary-confusion-matrix
  "makes a confusion matrix; assumes labels are 1 (positive) and 0 (negative)
   input: sequence of estimated labels, sequence of true labels from dataset (only labels!)
   output: map with confusion matrix elements"
  [estimated-class true-class]
  (let [true-positives  (apply + (map #(if (and (= 1 %1) (= 1 %2))
                                         1 0) estimated-class true-class))
        false-positives (apply + (map #(if (and (= 1 %1) (not (= 1 %2)))
                                         1 0) estimated-class true-class))
        true-negatives  (apply + (map #(if (and (not (= 1 %1)) (not (= 1 %2)))
                                         1 0) estimated-class true-class))
        false-negatives (apply + (map #(if (and (not (= 1 %1)) (= 1 %2))
                                         1 0) estimated-class true-class))]
    (assoc {} :tp true-positives :fp false-positives
              :tn true-negatives :fn false-negatives)))

(defn evaluate
  "calculates several model performance metrics; if only negative class is predicted,
  precision, recall and f1-score have the risk of dividing by zero, and in that case,
  they return 0.0, as those are positive class measures
  input: tree, coll or seq of observations
  output: map of metrics"
  [tree data]
  (let [estimated-class (map #(dtree/classify tree %) data)
        true-class      (map :result data)
        count           (count estimated-class)
        {:keys [tp fp tn fn]} (binary-confusion-matrix estimated-class true-class)
        accuracy        (double (/ (+ tp tn) count))
        error           (- 1 accuracy)
        precision       (if (or (< 0 tp) (< 0 fp)) (double (/ tp (+ tp fp))) 0.0)
        recall          (if (or (< 0 tp) (< 0 fn)) (double (/ tp (+ tp fn))) 0.0)
        f1-score        (if (or (< 0 precision) (< 0 recall))
                          (double (/ (* 2 precision recall) (+ precision recall))) 0.0)]
    (assoc {} :accuracy accuracy :error error
              :precision precision :recall recall
              :f1-score f1-score)))



(defn make-k-fold
  "makes an i-th split into training and validation datasets
  input data is assumed to be shuffled, and also separate from test subset;
  k is total number of iterations/splits, i is the current one
  output: {:validation-data [data] :train-data [data]}"
  [data k i]
  (let [n               (count data)
        start-idx       (/ (* i n) k)
        end-idx         (/ (* (inc i) n) k)
        validation-data (subvec data start-idx end-idx)
        train-data      (vec (concat (subvec data 0 start-idx)
                                     (subvec data end-idx)))]
    (assoc {} :validation-data validation-data
              :train-data train-data)))

(defn k-fold-cross-validation
  "makes k iterations of train-evaluate process and returns the average metric for the model
  input: classifier - function that wraps tree building with all the parameters except for data
  data - coll or seq of data rows; k - number f splits/iterations; metric - one of the metrics
  provided by evaluate fn
  output: average metric over all splits"
  [classifier data k metric]
  (->> (for [i (range k)
             :let [{:keys [train-data validation-data]} (make-k-fold (vec data) k i)
                   tree       (classifier train-data)
                   avg-metric (metric (evaluate tree validation-data))]]
             avg-metric)
       (apply +)
       (* (double (/ 1 k)))))
