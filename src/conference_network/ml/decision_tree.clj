(ns conference-network.ml.decision-tree)



(defn square [n]
  (* n n))

(defn operator
  "returns the appropriate operator for the type of data that is to be compared"
  [value]
  (if (not-any? #(instance? % value) [Long Double Float Integer Number])
    '=
    '>=))


(defn divide-set
  "divides input data into two sets
  input: rows: list of maps, column: key, value: number or string
  output: map {true [rec1 rec2], false [rec3]}"
  [rows column value]
  (let [operator (operator value)]
     (group-by #((eval operator) (column %) value) rows)))


(defn unique-counts
  "creates counts of different results (the last column) for the set of rows
  input: rows/observations of variables and results {:1 v1 :2 v2 :result res}
  output: sequence of maps {:result x :count n}"
  [rows]
  (->> (group-by :result rows)
       (map #(assoc {} :result (key %) :count (count (val %))))))


(defn gini-impurity
  "calculates Gini impurity for a set of results - probability that
  a randomly placed item will be in the wrong category
  by formula: 1 - (sum (pi)^2)
  input: rows (maps of observations) OR total-labels-count, each-label-counts (as from unique-counts)
  output: double"
  ([rows]
   (let [total  (count rows)
        counts (unique-counts rows)]
     (gini-impurity total counts)))
  ([total counts]
    (->> (map #(square (double (/ (:count %) total))) counts)
         (apply +)
         (- 1))))


(defn distinct-feature-values
  "finds all unique combinations of column - value (in matrix terms), which is
  actually implemented as key-value; used for finding best value to split the tree
  filters out :result column
  input: rows of maps {:columnx valuex ...} (like matrix rows)
  output: seq of distinct [:column value] combinations"
  [rows]
  (->> (reduce #(into %1 (seq %2)) #{} rows)
       (filter #(not (= :result (key %)))))) ;should removing result be in building tree fn?


(defn find-best-split
  "applies all distinct splits and finds the one with max gains; associates additional information to
  result for further computations & passing to other functions
  input: rows/observations, column-value combinations to split on, score function
  output: a map: {true [rows] false [rows] :split-on [:col value] :p x :gain y}"
  [rows combos score-fn]
  (let [row-count     (count rows)
        current-score (score-fn rows)
        all-divisions (map #(assoc (divide-set rows (key %) (val %)) :split-on %) combos)
        real-splits   (filter #(and (get % true) (get % false)) all-divisions)]
    ;^make all divisions and associate column-value combination to each
    ;^ get only those that truly split data into 'true' and 'false' subsets
    (if (empty? real-splits)
      (assoc {} :leaf rows :gain 0.0)                       ;if data cannot be divided by any value, gain is 0, make leaf
      (->> (map #(assoc % :p (double (/ (count (get % true)) row-count))) real-splits) ; calculate p and assoc it, for next step
           (map #(assoc % :gain                             ; assoc calculated gain for each
                          (- current-score
                             (* (:p %) (score-fn (get % true))) ;scores weighted by proportions of subsets
                             (* (- 1 (:p %)) (score-fn (get % false))))))
           (reduce #(if (> (:gain %1) (:gain %2))           ; return split with max :gain
                      %1
                      %2))
           (#(dissoc % :p))))))


(defn branch
  "branch node; makes a map with data used for branching, and children
  input: output of find-best-split function
  output: map"
  [split-data children]
  {:column (first (:split-on split-data))
   :value (second (:split-on split-data))
   :branch-true (first children)
   :branch-false (second children)})

(defn leaf
  "leaf node;
  input: empty dataset or split dataset
  3 cases: data has no rows - returns empty map
  data belongs to one category (marked by :leaf) - returns counts
  data is split, but no division leads to gains - returns counts for all categories"
  [data]
  (if (or (= data {}) (= data [{}]) (= data []) (= data '()))
    {}
    (let [result-data (flatten (vals (dissoc data :gain :split-on)))]
      (assoc {} :results (unique-counts result-data)))))


(defn filter-out-empty-rows
  [rows]
  (filter not-empty (flatten rows)))


(defn build-tree
  "builds a tree of maps
  input: rows/observations, optionally score function
  output: tree structure of maps"
  ([rows]
   (build-tree rows gini-impurity))
  ([rows score-fn]
   (let [rows (filter-out-empty-rows rows)]
    (if (= 0 (count rows))
      (leaf rows)
      (let [col-val-combos (distinct-feature-values rows)      ;all the combos to check
            best-split (find-best-split rows col-val-combos score-fn)]
        (if (> (:gain best-split) 0.0)
          (let [true-split (get best-split true)
                false-split (get best-split false)]
            (branch best-split (map #(build-tree % gini-impurity) [true-split false-split])))
          (leaf best-split)))))))


(defn classify
  "predicts class for one new observation; class is determined by majority vote
  input: tree, new observation, return probability with class?
  output: either only class (string/number) or {:class c :probability p}"
  ([tree row]
   (classify tree row false))
  ([tree row with-probability?]
   (if (empty? tree)
     nil
     (if (:results tree)
       (let [results        (:results tree)
             best-guess (apply max-key :count results)
             class          (:result best-guess)
             total-counts   (apply + (reduce #(conj %1 (:count %2)) () results))
             probability    (double (/ (:count best-guess) total-counts))
             classification (assoc {} :class class :probability probability)]
         (if with-probability? classification (:class classification)))
       (let [operator (operator (:value tree))]
         (if ((eval operator) ((:column tree) row) (:value tree))
           (classify (:branch-true tree) row with-probability?)
           (classify (:branch-false tree) row with-probability?)))))))


(defn merge-leaves-results
  "merges leaves results and sums them if needed
  input: sequences of results for two nodes: '({:result label :count n})
  output sequence of results for merged node"
  [true-node-results false-node-results]
  (->> (flatten (merge true-node-results false-node-results))
       (map #(assoc {} (:result %) (:count %)))
       (apply merge-with +)
       (map #(assoc {} :result (key %) :count (val %)))))


(defn should-be-pruned?
  "checks if information gain from branching is less than provided minimum; if so,
  branches (really leaves) should be pruned
  input: two nodes to be pruned potentially, minimum gain, score function
  output: true/false"
  [true-branch false-branch min-gain score-fn]
  (let [true-results (:results true-branch)
        false-results (:results false-branch)
        parent-node (merge-leaves-results true-results false-results)
        parent-total (apply + (map :count parent-node))
        true-total (apply + (map :count true-results))
        false-total (apply + (map :count false-results))
        score-parent (score-fn parent-total parent-node)
        score-true (score-fn true-total true-results)
        score-false (score-fn false-total false-results)
        gain (double (- score-parent (/ (+ score-true score-false) 2)))]
    (< gain min-gain)))


(defn prune
  "prunes the tree recursively, if gain from branching is less than min-gain
   input: tree, minimum gain, score function
   output: pruned tree"
  [tree min-gain score-fn]
  (if (contains? tree :results)
    tree
    (let [true-branch  (prune (:branch-true tree) min-gain score-fn)
          false-branch (prune (:branch-false tree) min-gain score-fn)]
      (if (and (contains? true-branch :results)
               (contains? false-branch :results))
        (if (should-be-pruned? true-branch false-branch min-gain score-fn)
          (-> (dissoc tree :branch-true :branch-false)
              (assoc :results
                     (merge-leaves-results (:results true-branch) (:results false-branch))))
          tree)
        tree))))

