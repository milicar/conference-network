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
  !!possibly returns 0 test rows, which produces /0 error later
  throws exception!!"
  ([data ratio]
   (let [seed (* 1000 (rand))]
     (divide-data data ratio seed)))
  ([data ratio seed]
   (let [shuffled-data (deterministic-shuffle data seed)
         n             (long (* ratio (count data)))
         test-data     (subvec shuffled-data 0 n)
         train-data    (subvec shuffled-data n)]
     (if (< 0 (count test-data))
       {:test-data test-data :train-data train-data}
       (throw (Exception. "No data in test!"))))))


(defn evaluate
  [tree data])


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
  "makes k iterations of train-evaluate process and returns the average error for the model
  input: classifier - function that wraps tree building with all the parameters except for data
  data - vector of data rows; k - number f splits/iterations
  output: average error over all splits"
  [classifier data k]
  (->> (for [i (range k)
             :let [folds           (make-k-fold data k i)
                   train-data      (:train-data folds)
                   validation-data (:validation-data folds)
                   tree            (classifier train-data)
                   error           (evaluate tree validation-data)]]
         error)
       (apply +)
       (* (double (/ 1 k)))))
