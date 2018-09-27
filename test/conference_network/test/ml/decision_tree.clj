(ns conference-network.test.ml.decision-tree
  (:require [conference-network.ml.decision-tree :as dtree]
            [midje.sweet :refer :all]
            [java.lang]))


(facts "divide-set function divides input maps into two sets, based on key-value"
       (fact "by this point, data has been checked for empty rows and missing values,
              so testing only happy paths"
             (let [rows [{:k1 11 :k2 12 :k3 13}
                         {:k1 21 :k2 22 :k3 23}
                         {:k1 31 :k2 32 :k3 33}]]
               (dtree/divide-set rows :k2 22) => {true [{:k1 21 :k2 22 :k3 23}
                                                           {:k1 31 :k2 32 :k3 33}]
                                                   false [{:k1 11 :k2 12 :k3 13}]})
             (let [rows [{:k1 "a" :k2 "aa" :k3 "aaa"}
                         {:k1 "b" :k2 "bb" :k3 "bbb"}
                         {:k1 "c" :k2 "cc" :k3 "ccc"}]]
               (dtree/divide-set rows :k1 "a") => {true [{:k1 "a" :k2 "aa" :k3 "aaa"}]
                                                   false [{:k1 "b" :k2 "bb" :k3 "bbb"}
                                                          {:k1 "c" :k2 "cc" :k3 "ccc"}]})))

(comment "Implement validating input data!
Check for missing keys and values, empty maps and 0 maps (0 rows)
 That should be handled up in the call stack, not here..")
       ;(fact "non-existent key throws NullPointerException, but keys are extracted from rows anyway"
       ;;      (let [rows [{:k1 11 :k2 12 :k3 13}
       ;;                  {:k1 21 :k2 22 :k3 23}
       ;;                  {:k1 31 :k2 32 :k3 33}]]
       ;;        (dtree/divide-set rows :k4 22) => (throws NullPointerException)))
       ;;(fact "non-existent values are no problem"
       ;;      (let [rows [{:k1 11 :k2 12 :k3 13}
       ;;                  {:k1 21 :k2 22 :k3 23}
       ;;                  {:k1 31 :k2 32 :k3 33}]]
       ;;        (dtree/divide-set rows :k2 19) => {true [{:k1 21 :k2 22 :k3 23}
       ;;                                                 {:k1 31 :k2 32 :k3 33}]
       ;;                                           false [{:k1 11 :k2 12 :k3 13}]}))
       ;;(fact "non-existent values of type string puts all observations into same group"
       ;;      (let [rows [{:k1 "a" :k2 "aa" :k3 "aaa"}
       ;;                  {:k1 "b" :k2 "bb" :k3 "bbb"}
       ;;                  {:k1 "c" :k2 "cc" :k3 "ccc"}]]
       ;;        (dtree/divide-set rows :k1 "d") => {false [{:k1 "a" :k2 "aa" :k3 "aaa"}
       ;;                                                   {:k1 "b" :k2 "bb" :k3 "bbb"}
       ;;                                                   {:k1 "c" :k2 "cc" :k3 "ccc"}]}))
