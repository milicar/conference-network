(ns conference-network.models.graph
  (:require [ubergraph.core :as g]))


(defn add-nodes
  [graph nodes-map]
  (g/add-nodes-with-attrs* graph nodes-map))


(defn parse-edges
  "parses edges for one source from format [:source {:dest1 weigth1, :dest2 weigth2}] to
  [:source :dest {:weigth x}]"
  [edges]
  (reduce #(conj %1 [(first edges) (first %2) {:weigth (second %2)}]) [] (second edges)))

(defn add-edges
  [graph edges-map]
  (let [edges (filter #(not (empty? (val %))) edges-map)]
    (->> edges
         (map parse-edges)
         (apply concat)
         (g/add-directed-edges* graph))))

(defn make-graph
  [elements]
  (let [graph (g/digraph)]
    (-> graph
        (add-nodes (:nodes elements))
        (add-edges (:edges elements)))))
