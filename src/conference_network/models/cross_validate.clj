(ns conference-network.models.cross-validate
  (:require [conference-network.models.decision-tree :as dtree]))

(defn divide-data
  "possibly returns 0 test rows, which produces /0 error
  it should be fixed here, and not subtract nil counts elsewhere in calculations
  "
  ([data]
   (divide-data data 0.05))
  ([data test-ratio]
   (let [test-set (set (random-sample test-ratio (set data)))
         train-set (clojure.set/difference (set data) test-set)]
       {:test-set test-set :train-set train-set})))


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

