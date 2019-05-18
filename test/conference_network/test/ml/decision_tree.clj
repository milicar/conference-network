(ns conference-network.test.ml.decision-tree
  (:require [conference-network.ml.decision-tree :as dtree]
            [midje.sweet :refer :all]))



(facts "distinct-feature-value returns a sequence of all distinct values a feature can have
        (and be divided on), for each feature"
       (fact "all ok for one, two of the same or multiple values"
             (dtree/distinct-feature-values [{:single 56.78}]) => '([:single 56.78])

             (dtree/distinct-feature-values [{:cx 1}
                                             {:cx 1}]) => '([:cx 1])

             (dtree/distinct-feature-values [{:c1 "a" :c2 1}
                                             {:c1 "b" :c2 2}]) => '([:c1 "a"] [:c1 "b"] [:c2 1] [:c2 2]))

       (fact "for no rows returns empty sequence! caller (tree building function) should never
              call it with no rows"
             (dtree/distinct-feature-values []) => '()))


(facts "unique-counts returns the number of times each result/target value apears in dataset"
       (fact "all is well for a happy path"
             (dtree/unique-counts [{:feature1 "val1" :result 1}
                                   {:feature1 "val2" :result 1}]) => '({:result 1 :count 2})
             (dtree/unique-counts [{:f1 1 :f2 2 :result "A"} {:f1 1 :f2 6 :result "A"}
                                   {:f1 1 :f2 3 :result "B"} {:f1 1 :f2 7 :result "B"}
                                   {:f1 1 :f2 4 :result "C"} {:f1 1 :f2 8 :result "D"}
                                   {:f1 1 :f2 5 :result "A"} {:f1 1 :f2 9 :result "C"}]) => '({:result "A" :count 3}
                                                                                               {:result "B" :count 2}
                                                                                               {:result "C" :count 2}
                                                                                               {:result "D" :count 1}))
       (fact "no rows or empty rows cause some strange behaviour, which impacts score fn as well"
             (dtree/unique-counts []) => '()
             (dtree/unique-counts [{}]) => '({:result nil :count 1})
             (dtree/unique-counts [{:f1 1 :result "a"} {} {}]) => '({:result "a" :count 1}
                                                                     {:result nil :count 2})))


(facts "gini-impurity returns the probability that a randomly placed item will
         be in the wrong category - the cleaner the split, the smaller that probability
         formula is: 1 - (sum (pi)^2) where pi=prob(i)= count(i)/count(all)"
       (fact "all ok for correct rows"
             (dtree/gini-impurity [{:f1 1 :result "a"}
                                   {:f1 2 :result "a"}]) => 0.0
             (dtree/gini-impurity [{:f1 1 :result 1} {:f1 2 :result 2}]) => 0.5
             (dtree/gini-impurity [{:f1 1 :result 1} {:f1 2 :result 1}
                                   {:f1 3 :result 2}]) => (- 1 (/ 5.0 9)))
       (fact "missing features is of no significance here"
             (dtree/gini-impurity [{:result 1} {:result 2}]) => 0.5)
       (fact "BEWARE of no rows and empty rows, because gini-impurity calls unique-counts!
              unique-counts returns 0 for no rows, but 1 for an empty row"
             (dtree/gini-impurity []) => 1                  ;1-0
             (dtree/gini-impurity [{}]) => 0.0              ;1-1.0
             (dtree/gini-impurity [{:f1 1 :result 1} {}]) => 0.5))



(facts "divide-set function divides input maps into two sets, based on key-value;
        it is called with data resulting from finding feature-value combinations, so
        it should't be called with non-existent feature/column;
        non-existent values are valid for splitting the observations on (they might be
        present in some other subset of observations);
        it may happen that a set of data cannot be split by some feature value, and there
        is only one subset (true or false)"

       (fact "by this point, data has been checked for empty rows and missing values,
              so testing only happy paths"
             (let [rows [{:c1 11 :c2 12 :c3 13}
                         {:c1 21 :c2 22 :c3 23}
                         {:c1 31 :c2 32 :c3 33}]]
               (dtree/divide-set rows :c2 22) => {true  [{:c1 21 :c2 22 :c3 23}
                                                         {:c1 31 :c2 32 :c3 33}]
                                                  false [{:c1 11 :c2 12 :c3 13}]})
             (let [rows [{:c1 "a" :c2 "aa" :c3 "aaa"}
                         {:c1 "b" :c2 "bb" :c3 "bbb"}
                         {:c1 "c" :c2 "cc" :c3 "ccc"}]]
               (dtree/divide-set rows :c1 "a") => {true  [{:c1 "a" :c2 "aa" :c3 "aaa"}]
                                                   false [{:c1 "b" :c2 "bb" :c3 "bbb"}
                                                          {:c1 "c" :c2 "cc" :c3 "ccc"}]})
             (let [rows [{:c1 "a" :c2 2 :c3 3.16}
                         {:c1 "b" :c2 22 :c3 33.45}
                         {:c1 "c" :c2 222 :c3 333.789}]]
               (dtree/divide-set rows :c3 33) => {true  [{:c1 "b" :c2 22 :c3 33.45}
                                                         {:c1 "c" :c2 222 :c3 333.789}]
                                                  false [{:c1 "a" :c2 2 :c3 3.16}]}))
       (fact "dividing on numerical values that do not exist in columns might divide
              the set to two or it might not; this is checked in find-best-split function"
             (let [rows [{:c1 "a" :c2 2 :c3 3}
                         {:c1 "b" :c2 22 :c3 33}
                         {:c1 "c" :c2 222 :c3 333}]]
               (dtree/divide-set rows :c3 55) => {true  [{:c1 "c" :c2 222 :c3 333}]
                                                  false [{:c1 "a" :c2 2 :c3 3}
                                                         {:c1 "b" :c2 22 :c3 33}]})
             (let [rows [{:c1 "a" :c2 2 :c3 3}
                         {:c1 "b" :c2 22 :c3 33}
                         {:c1 "c" :c2 222 :c3 333}]]
               (dtree/divide-set rows :c2 45.7) => {true  [{:c1 "c" :c2 222 :c3 333}]
                                                    false [{:c1 "a" :c2 2 :c3 3}
                                                           {:c1 "b" :c2 22 :c3 33}]})

             (let [rows [{:c1 "a" :c2 2 :c3 3}
                         {:c1 "b" :c2 22 :c3 33}
                         {:c1 "c" :c2 222 :c3 333}]]
               (dtree/divide-set rows :c2 0) => {true [{:c1 "a" :c2 2 :c3 3}
                                                       {:c1 "b" :c2 22 :c3 33}
                                                       {:c1 "c" :c2 222 :c3 333}]}))
       (fact "dividing on categorical values that do not exist in columns never divides
              the set into two - there is no 'true' subset; this is checked in find-best-split fn"
             (let [rows [{:c1 "a" :c2 2 :c3 3}
                         {:c1 "b" :c2 22 :c3 33}
                         {:c1 "c" :c2 222 :c3 333}]]
               (dtree/divide-set rows :c1 "x") => {false [{:c1 "a" :c2 2 :c3 3}
                                                          {:c1 "b" :c2 22 :c3 33}
                                                          {:c1 "c" :c2 222 :c3 333}]})))


(facts "find-best-split"
       (fact "if data cannot be split (e.g there's only one row, or points overlap),
              returns a signal to make a leaf"
             (let [rows [{:f1 1 :result "A"}]
                   feature-values (dtree/distinct-feature-values rows)
                   score-fn dtree/gini-impurity]
               (dtree/find-best-split rows feature-values score-fn) =>
               {:leaf [{:f1 1 :result "A"}], :gain 0.0})

             (let [rows [{:f1 1 :f2 "a" :result "A"}]
                   feature-values (dtree/distinct-feature-values rows)
                   score-fn dtree/gini-impurity]
               (dtree/find-best-split rows feature-values score-fn) =>
               {:leaf [{:f1 1 :f2 "a" :result "A"}], :gain 0.0})

             (let [rows [{:f1 1 :f2 "yes" :result "A"}{:f1 1 :f2 "yes" :result "B"}]
                   feature-values (dtree/distinct-feature-values rows)
                   score-fn dtree/gini-impurity]
               (dtree/find-best-split rows feature-values score-fn) =>
               {:leaf [{:f1 1 :f2 "yes" :result "A"}{:f1 1 :f2 "yes" :result "B"}], :gain 0.0}))

       (fact "if data can be split, returns the best split with calculated score for it"
             (let [rows [{:f1 1 :result "A"}{:f1 2 :result "B"}]
                   feature-values (dtree/distinct-feature-values rows)
                   score-fn dtree/gini-impurity
                   score-before-split (dtree/gini-impurity rows)
                   score-after-true (* 0.5 (dtree/gini-impurity [{:f1 2 :result "B"}]))
                   score-after-false (* 0.5 (dtree/gini-impurity [{:f1 1 :result "A"}]))]
               (dtree/find-best-split rows feature-values score-fn) =>
               (contains {:gain (- score-before-split score-after-false score-after-true) :split-on [:f1 2]}))

             (let [rows [{:f1 1 :result "A"}{:f1 2 :result "B"}{:f1 3 :result "C"}]
                   feature-values (dtree/distinct-feature-values rows)
                   score-fn dtree/gini-impurity
                   score-before-split (dtree/gini-impurity rows)
                   score-after-true (* (double (/ 2 3)) (dtree/gini-impurity [{:f1 2 :result "B"}{:f1 3 :result "C"}]))
                   score-after-false (* (double (/ 1 3)) (dtree/gini-impurity [{:f1 1 :result "A"}]))]
               (dtree/find-best-split rows feature-values score-fn) =>
               (contains {:gain (- score-before-split score-after-true score-after-false) :split-on [:f1 2]}))

             (let [rows [{:f1 1 :result "A"}{:f1 2 :result "B"}{:f1 3 :result "B"}]
                   feature-values (dtree/distinct-feature-values rows)
                   score-fn dtree/gini-impurity
                   score-before-split (dtree/gini-impurity rows)
                   score-after-true (* (double (/ 2 3)) (dtree/gini-impurity [{:f1 2 :result "B"}{:f1 3 :result "B"}]))
                   score-after-false (* (double (/ 1 3)) (dtree/gini-impurity [{:f1 1 :result "A"}]))]
               (dtree/find-best-split rows feature-values score-fn) =>
               (contains {:gain (- score-before-split score-after-true score-after-false) :split-on [:f1 2]})))
       (fact "in some cases, data can be split, but gain is 0"
             (let [rows [{:f1 1 :result "A"}{:f1 2 :result "A"}{:f1 1 :result "B"}{:f1 2 :result "B"}]
                   feature-values (dtree/distinct-feature-values rows)
                   score-fn dtree/gini-impurity]
               (dtree/find-best-split rows feature-values score-fn) =>
               (contains {:gain 0.0 :split-on [:f1 2]}))))


(facts "leaves: called from build-tree fn"
       (fact "making leaf from no rows (or empty rows):WHAT SHOULD THIS BEEEE?"
             (dtree/leaf '()) => {}
             (dtree/leaf []) => {}
             (dtree/leaf [{}]) => {})
       (fact "making leaf from one subset of rows - when observations could not be split"
             (dtree/leaf {:leaf [{:f1 1 :f2 "a" :result "A"}] :gain 0.0}) =>
             (contains {:results '({:result "A" :count 1})}))
       (fact "making leaf from two subsets, where observations were split, but gain was 0.0"
             (dtree/leaf {false [{:f1 1, :result "A"} {:f1 1, :result "B"}],
                          true [{:f1 2, :result "A"} {:f1 2, :result "B"}],
                          :split-on [:f1 2],
                          :gain 0.0}) =>
             (contains {:results '({:result "A" :count 2}{:result "B" :count 2})})))


(facts "branch makes a map of split data; it actually calls build-tree recursively for left and right split"
       (fact "this shouldn't be anything but the happy path, it's called only if there
              are a true and a false splits and if gain is acceptable"
             (let [best-split {false [{:f1 1, :result "A"}],
                               true [{:f1 2, :result "B"} {:f1 3, :result "B"}],
                               :split-on [:f1 2],
                               :gain 0.4444444444444444}]
             (dtree/branch best-split (map #(get best-split %) [true false])) =>
             {:column :f1 :value 2
              :branch-true [{:f1 2, :result "B"} {:f1 3, :result "B"}]
              :branch-false [{:f1 1, :result "A"}]})))


(facts "build-tree"
       (fact "no rows or empty rows result in just an empty map"
             (dtree/build-tree {}) => {}
             (dtree/build-tree []) => {}
             (dtree/build-tree [{}]) => {}
             (dtree/build-tree [{}{}{}]) => {})
       (fact "if data cannot be split, tree consists only of one leaf"
             (dtree/build-tree [{:f1 1 :result "A"}]) => {:results '({:result "A" :count 1})}
             (dtree/build-tree [{:f1 1 :result "A"}
                                {:f1 1 :result "B"}]) => {:results '({:result "A" :count 1}
                                                                     {:result "B" :count 1})})
       (fact "if data can be split, tree has branches"
             (dtree/build-tree [{:f1 1 :result "no"}{:f1 2 :result "yes"}]) =>
             {:column :f1 :value 2
              :branch-true {:results '({:result "yes" :count 1})}
              :branch-false {:results '({:result "no" :count 1})}}

             (dtree/build-tree [{:f1 1 :result "A"}{:f1 2 :result "B"}{:f1 3 :result "C"}]) =>
             {:column :f1 :value 2
              :branch-true {:column :f1 :value 3
                            :branch-true {:results '({:result "C" :count 1})}
                            :branch-false {:results '({:result "B" :count 1})}}
              :branch-false {:results '({:result "A" :count 1})}}))



(facts "empty rows should be filtered out"
       (dtree/filter-out-empty-rows {}) => '()
       (dtree/filter-out-empty-rows []) => '()
       (dtree/filter-out-empty-rows [{}]) => '()
       (dtree/filter-out-empty-rows [{}{}{}]) => '()
       (dtree/filter-out-empty-rows [{:k 1}{}{}{:k 2}]) => '({:k 1}{:k 2}))

