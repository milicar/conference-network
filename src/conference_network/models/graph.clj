(ns conference-network.models.graph
  (:require [ubergraph.core :as g]))


(defn add-nodes
  [graph nodes-map]
  (g/add-nodes-with-attrs* graph nodes-map))


(defn parse-edges
  "parses edges for one source from format [:source {:dest1 weight1, :dest2 weight2}] to
  [:source :dest1 {:weight 1}][:source :dest2 {:weight 2}]"
  [edges]
  (reduce #(conj %1 [(first edges) (first %2) {:weight (second %2)}]) [] (second edges)))

(defn add-edges
  [graph edges-map]
  (->> edges-map
       (map parse-edges)
       (apply concat)
       (g/add-directed-edges* graph)))

(defn make-graph
  [elements]
  (let [graph (g/digraph)]
    (-> graph
        (add-nodes (:nodes elements))
        (add-edges (:edges elements)))))

(defn serialize-graph
  [graph]
  (binding [*print-dup* true] (pr-str graph)))

(defn deserialize-graph
  [graph-string]
  (read-string graph-string))