(ns conference-network.ml.cross-validate
  (:require [conference-network.ml.decision-tree :as dtree]))


(defn deterministic-shuffle
  "Return a random permutation of coll"
  [^java.util.Collection coll seed]
  (let [array-list (java.util.ArrayList. coll)
        rand-generator (java.util.Random. seed)]
    (java.util.Collections/shuffle array-list rand-generator)
    (clojure.lang.RT/vector (.toArray array-list))))


(defn divide-data
  "takes random samples for test and validation datasets; uses deterministic shuffle,
  for reproducibility (if seed is not provided, shuffle is randomised by random seed);
  after shuffle, saves the order of data
  !!possibly returns 0 test rows, which produces /0 error later
  throws exception!!
  "
  ([data ratio]
   (let [seed (* 1000 (rand))]
     (divide-data data ratio seed)))
  ([data ratio seed]
   (let [shuffled-data (deterministic-shuffle data seed)
         n (long (* ratio (count data)))
         test-data (subvec shuffled-data 0 n)
         train-data (subvec shuffled-data n)]
     (if (< 0 (count test-data))
       {:test-data test-data :train-data train-data}
       (throw (Exception. "No data in test!"))))))


(defn test-classifier
  [classifier trainset testset]
  (let [tree (dtree/build-tree trainset)] ;dear lord.. such coupled.. wow..
    (->> (map #(if (= (:result %) (:result (classifier tree %)))
            1 0) testset)
         (apply +)
         (#(/ % (count testset)))
         (double))))


(defn cross-validate
  ([classifier data]
   (cross-validate classifier data 100 0.05))
  ([classifier data trials test-ratio]
   (->> (repeatedly trials
               (fn []
                 (let [datasets (divide-data data test-ratio)
                       trainset (:train-set datasets)
                       testset (:test-set datasets)]
                   (test-classifier classifier trainset testset))))
        (apply +)
        (#(/ % trials))
        (double))))

