(ns conference-network.test.ml.cross-validate
  (:require [conference-network.ml.cross-validate :as cv]
            [midje.sweet :refer :all]))


(facts "divide data into two parts (vectors) determined by a ratio and a seed"
       (let [data (range 500)]
         (fact "providing ratio and seed"
               (cv/divide-data data 0.2 0) => (cv/divide-data data 0.2 0)
               (cv/divide-data data 0.1 1) =not=> (cv/divide-data data 0.1 2)))
       (let [data (vec (map #(assoc {} (keyword (str %)) %) (range 500)))]
         (fact "maps as data work as well"
               (cv/divide-data data 0.5 0) => (cv/divide-data data 0.5 0)
               (cv/divide-data data 0.5 100) =not=> (cv/divide-data data 0.5 20))
         (fact "not providing seed gives potentially different result every time -
                seed is randomly generated"
               (cv/divide-data data 0.5) =not=> (cv/divide-data data 0.5)))
       ;(let [data [0 1 2]]
       ;  (fact "if ratio is too small for amount of data, and there is no data
       ;         in test subset, exception is thrown"
       ;        (cv/divide-data data 0.1) =throws=> (Exception.)))
       )

(facts "make-k-fold makes one iteration of dividing dataset into validation
        and train datasets; it is called by for [i (range k)]"
       (fact "when split is clean, all folds are equal"
             (for [i (range 3)]
               (cv/make-k-fold (vec (range 12)) 3 i)) =>
             '({:validation-data [0 1 2 3] :train-data [4 5 6 7 8 9 10 11]}
                {:validation-data [4 5 6 7] :train-data [0 1 2 3 8 9 10 11]}
                {:validation-data [8 9 10 11] :train-data [0 1 2 3 4 5 6 7]}))
       (fact "when n is not a multiple of k, folds are not equal, but all data
              points are accounted for"
             (for [i (range 3)]
               (cv/make-k-fold (vec (range 11)) 3 i)) =>
             '({:validation-data [0 1 2] :train-data [3 4 5 6 7 8 9 10]}
                {:validation-data [3 4 5 6] :train-data [0 1 2 7 8 9 10]}
                {:validation-data [7 8 9 10] :train-data [0 1 2 3 4 5 6]})))


(facts "k-fold-cross-validation returns the average of metrics from k evaluations of
        the model; the metric to be averaged has to be specified"
       (let [data (vec (range 50))]
         (fact "evaluate is mocked to always return the same value per metric, = average"
               (cv/k-fold-cross-validation anything data 10 :accuracy) => 0.5
               (cv/k-fold-cross-validation anything data 10 :f1-score) => 0.75))
       (against-background (cv/evaluate anything anything) => {:accuracy 0.5 :f1-score 0.75}))


(facts "binary confusion matrix assumes class labels are encoded as 1 and 0"
       (let [estimated-class '(1 0 1 1 0 0 0 1)
             true-class      '(1 1 1 1 0 0 0 0)]
         (cv/binary-confusion-matrix estimated-class true-class) =>
         {:tp 3 :fp 1 :tn 3 :fn 1}))

(facts "evaluate returns several metrics for the decision tree,
        calculated on confusion matrix"
       (let [tree             {:column       :f1 :value 2
                               :branch-true  {:column       :f1 :value 3
                                              :branch-true  {:results '({:result 0 :count 1})}
                                              :branch-false {:results '({:result 1 :count 1})}}
                               :branch-false {:results '({:result 0 :count 1})}}
             data             '({:f1 1 :result 0} {:f1 2 :result 1} {:f1 5 :result 1} {:f1 1 :result 1})
             estimated-class  '(0 1 0 0)
             confusion-matrix {:tp 1 :fp 0 :tn 1 :fn 2}]
         (cv/evaluate tree data) => (contains {:accuracy  0.5 :error 0.5
                                               :precision 1.0 :recall (roughly 0.3 0.1)
                                               :f1-score  (roughly 0.5 0.1)}))   )

