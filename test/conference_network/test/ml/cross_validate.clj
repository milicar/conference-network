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





