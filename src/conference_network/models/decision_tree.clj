(ns conference-network.models.decision-tree)



(defn square [n]
  (* n n))

(defn op
  [value]
  (if (not-any? #(instance? % value) [Long Double Float Integer Number])
    '=
    '>=))


(defn divide-set
  "divides input data into two sets
  input: rows: list of maps, column: key, value: number or string
  output: map {true [rec1 rec2], false [rec3]}"
  [rows column value]
  (let [op (op value)]
     (group-by #((eval op) (column %) value) rows)))


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
  input: rows (maps of observations)
  output: double"
  [rows]
  (let [total  (count rows)
        counts (unique-counts rows)]
    (->> (map #(square (double (/ (:count %) total))) counts)
         (apply +)
         (- 1))))


(defn column-value-combos
  "finds all unique combinations of column - value (in matrix terms), which is
  actually implemented as key-value; used for finding best value to split the tree
  filters out :result column
  input: rows of maps {:columnx valuex ...} (like matrix rows)
  output: seq of distinct [:column value] combinations"
  [rows]
  (->> (reduce #(into %1 (seq %2)) #{} rows)
       (filter #(not (= :result (key %))))))


(defn find-best-split
  "applies all distinct splits and finds the one with max gains; associates additional information to
  result for further computations & passing to other functions -> possibly an indication of a bad design
  input: rows/observations, column-value combinations to split on, score function
  output: a map: {true [rows] false [rows] :combo [:col value] :p x :gain y}"
  [rows combos score-fn]
  (let [row-count (count rows)
        current-score (score-fn rows)
        all-divisions (map #(assoc (divide-set rows (key %) (val %)) :combo %) combos)
        multi-categoried (filter #(and (get % true) (get % false)) all-divisions)]
    ;^make all divisions and associate column-value combination to each
    ;^ get only those that have mix of categories
    (if (empty? multi-categoried)
      (assoc {} :leaf rows :gain 0)                ;if data cannot be divided, gain is 0, make leaf
      (->> (map #(assoc % :p (double (/ (count (get % true)) row-count))) multi-categoried) ; calculate p and assoc, for next step
               (map #(assoc % :gain                                             ; assoc calculated gain for each
                              (- current-score
                                 (* (:p %) (score-fn (get % true)))             ;product of weighted p-s for both categories
                                 (* (- 1 (:p %)) (score-fn (get % false))))))
               (reduce #(if (> (:gain %1) (:gain %2))                   ; return record with max :gain
                          %1
                          %2) )))) )
;
;(defn decision-node
;  "creates a decision node, constructor"
;  [& args]
;  (let [{:keys [column value result fbranch tbranch]
;         :or {column -1 value nil result nil fbranch nil tbranch nil}} args]
;    {:column column :value value :result result :fbranch fbranch :tbranch tbranch}))


(defn branch
  "branch node; makes a map with data used for branching, and children
  input: output of find-best-split function
  output: map"
  [data children]
  {:column (key (:combo data)) :value (val (:combo data)) :children children})

(defn leaf
  "leaf node;
  input: empty dataset or split dataset
  3 cases: data has no rows - returns empty map
  data belongs to one category (marked by :leaf) - returns counts
  data is mixed, but no division leads to gains - returns counts for all categories"
  [data]
  (if-let [counts (:leaf data)]
    (into {} (unique-counts counts))
    (into {} (unique-counts (apply conj (get data true) (get data false))))))

(defn build-tree
  "builds a tree of maps
  input: rows/observations, optionally score function
  output: tree structure of maps"
  ([rows]
   (build-tree rows gini-impurity))
  ([rows score-fn]
    (if (= 0 (count rows))
      (leaf rows)
      (let [col-val-combos (column-value-combos rows)      ;all the combos to check
            best-split (find-best-split rows col-val-combos score-fn)]
        (if (> (:gain best-split) 0 )
          (let [true-split (get best-split true)
                false-split (get best-split false)]
            (branch best-split (map #(build-tree % gini-impurity) [true-split false-split])))
          (leaf best-split))))))



(defn predict
  "predicts class for one new observation; does not update the tree
  input: tree, new observation
  output: {:result res :count n}"
  [tree row]
  (if (:result tree)
    tree
    (let [operator (op (:value tree))]
      (if ((eval operator) ((:column tree) row) (:value tree))
        (predict (first (:children tree)) row)
        (predict (second (:children tree)) row)))))

;
;(defn prune
;  [tree mingain]
;  (let [children (:children tree)]
;    (when (contains? (first children) :column)
;      (prune (first children) mingain))
;    (when (contains? (second children) :column)
;      (prune (second children) mingain))
;    (when (and (contains? (first children) :result)
;               (contains? (second children) :result))
;      (let [true-br (first children)
;            true-rows (repeat (:count true-br) true-br)
;            false-br (second children)
;            false-rows (repeat (:count false-br) false-br)
;            all-rows (flatten (conj true-rows false-rows))
;            delta (- (gini-impurity all-rows)            ;???
;                     (/ (+ (gini-impurity true-rows)
;                           (gini-impurity false-rows)) 2))]
;        (when (< delta mingain)
;          (unique-counts all-rows))))))
      ; recreate the whole tree except this part.. ??




; SAMPLE DATA - from Programming Collective Intelligence
(def data
  [{:1 "slashdot" :2 "USA" :3 "yes" :4 18 :result "none"}
  {:1 "google" :2 "France" :3 "yes" :4 23 :result "premium"}
  {:1 "digg" :2 "USA" :3 "yes" :4 24 :result "basic"}
  {:1 "kiwi" :2 "France" :3 "yes" :4 23 :result "basic"}
  {:1 "google" :2 "UK" :3 "no" :4 21 :result "premium"}
  {:1 "direct" :2 "New Zealand" :3 "no" :4 12 :result "none"}
  {:1 "direct" :2 "UK" :3 "no" :4 21 :result "basic"}
  {:1 "google" :2 "USA" :3 "no" :4 24 :result "premium"}
  {:1 "slashdot" :2 "France" :3 "yes" :4 19 :result "none"}
  {:1 "digg" :2 "USA" :3 "no" :4 18 :result "none"}
  {:1 "google" :2 "UK" :3 "no" :4 18 :result "none"}
  {:1 "kiwi" :2 "UK" :3 "no" :4 19 :result "none"}
  {:1 "digg" :2 "New Zealand" :3 "yes" :4 12 :result "basic"}
  {:1 "slashdot" :2 "UK" :3 "no" :4 21 :result "none"}
  {:1 "google" :2 "UK" :3 "yes" :4 18 :result "basic"}
  {:1 "kiwi" :2 "France" :3 "yes" :4 19 :result "basic"}])


;(def initialize-tree (build-tree data))

