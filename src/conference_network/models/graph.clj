(ns conference-network.models.graph
  (:require [ubergraph.core :as g]
            [loom.alg]))


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


(defn get-node-degree-centralities
  "returns map of node id, in-degree and out-degree"
  [graph]
  (let [node-count (g/count-nodes graph)]
    (map #(assoc {}
          :id (identity %)
          :in-degree-centr (/ (g/in-degree graph %) node-count)
          :out-degree-centr (/ (g/out-degree graph %) node-count))
       (g/nodes graph))))


(defn clique-belonging-nodes
  "for each node in graph, assigns 1 if it belongs to a clique,
  0 otherwise;
  input: graph
  output: map {:id id :in-clique x}"
  [graph]
  (let [all-cliques (->> (loom.alg/maximal-cliques graph)
                        (filter #(> (count %) 1)))
        all-nodes (apply clojure.set/union all-cliques)]
    (map #(assoc {}
            :id (identity %)
            :in-clique (if (contains? all-nodes (identity %)) 1 0))
         (g/nodes graph))))

