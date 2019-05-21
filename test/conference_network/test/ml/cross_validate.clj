(ns conference-network.test.ml.cross-validate
  (:require [conference-network.ml.cross-validate :as cv]
            [midje.sweet :refer :all]))


(facts "divide data into two parts (vectors) determined by a ratio and a seed"
       (let [data (range 500)]
         (fact "providing ratio and seed"
               (cv/divide-data data 0.2 0) => (cv/divide-data data 0.2 0)
               (cv/divide-data data 0.1 1) =not=> (cv/divide-data data 0.1 2) ))
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


(facts "k-fold-cross-validation returns the average error of k evaluations of
        the model"
       (let [data (vec (range 50))]
         (fact "evaluate is mocked to always return the same value, = average"
           (cv/k-fold-cross-validation anything data 10) => 0.5))
       (against-background (cv/evaluate anything anything) => 0.5))



