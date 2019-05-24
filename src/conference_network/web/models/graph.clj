(ns conference-network.web.models.graph
  (:require [ubergraph.core :as ug]
            [loom.alg]
            [conference-network.web.models.jungerer-graph :as jg]
            [conference-network.ml.decision-tree :as dtree]))

; main ns for functions dealing with graphs
; some of the functions call conference-network.web.models.jungerer-graph ns

(defn add-nodes
  [graph nodes-map]
  (ug/add-nodes-with-attrs* graph nodes-map))

; This structure is the result of tweets/update-edges-mentions and tweets/update-edges-replies;
; It has to be transformed here for ubergraph's function for adding edges
; If this map structure has to be transformed elsewhere, it might be ok to refactor and change it in said fs
(defn parse-edges
  "parses edges for one source from format [:sourceID {:dest1ID weight1, :destID2 weight2}] to
  [:sourceID :dest1ID {:weight 1}][:sourceID :dest2ID {:weight 2}]"
  [edges]
  (reduce #(conj %1 [(first edges) (first %2) {:weight (second %2)}]) [] (second edges)))

(defn add-edges
  [graph edges-map]
  (->> edges-map
       (map parse-edges)
       (apply concat)
       (ug/add-directed-edges* graph)))

(defn make-graph
  "builds an ubergraph's directed graph structure
  elements for graph are supplied by tweets-parsing functions in conference-network.web.models.tweets ns"
  [elements]
  (let [graph (ug/digraph)]
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
  "returns map of node id, in-degree and out-degree
  as proportions of total number of nodes"
  [graph]
  (let [node-count (ug/count-nodes graph)]
    (map #(assoc {}
          (identity %)
          (vector (double (/ (ug/in-degree graph %) node-count))
                  (double (/ (ug/out-degree graph %) node-count))))
       (ug/nodes graph))))

(defn get-node-betweenness-centralities
  "for each node, finds betweenness centrality score
  input: ubergraph
  output: sequence of maps: {:node centrality-score}"
  [graph]
  (jg/betweenness-centralities graph))


(defn get-node-closeness-centralities
  "for each node, finds closeness centrality score
  input: ubergraph
  output: sequence of maps: {:node centrality-score}"
  [graph]
  (jg/closeness-centralities graph))


(defn get-node-eigenvector-centralities
  "for each node, finds eigenvector centrality score
  input: ubergraph
  output: sequence of maps: {:node centrality-score}"
  [graph]
  (jg/eigenvector-centralities graph))


(defn get-node-pagerank-centralities
  "for each node, finds pagerank centrality score
  input: ubergraph
  output: sequence of maps: {:node centrality-score}"
  [graph]
  (jg/pagerank-centralities graph))


(defn get-weak-components-nodes
  "for each node, assigns 1 if it belongs to a weak component (graph is directed!),
   0 if not
  input: ubergraph
  output: sequence of maps: {:node 0/1}"
  [graph]
  (jg/nodes-by-weak-components graph))


(defn get-eb-clusters-nodes
  "for each node, assigns 1 if it belongs to any cluster, 0 if not
  input: ubergraph, number of nodes to remove
  output: sequence of maps: {:node 0/1}"
  [graph remove-n-nodes]
  (jg/nodes-by-edge-betweenness-clusters graph remove-n-nodes))


;;; halted on one occasion, needs further checking
(defn clique-belonging-nodes
  "for each node in graph, assigns 1 if it belongs to a clique,
  0 otherwise;
  input: graph
  output: map {:nodeID 0/1}"
  [graph]
  (let [all-cliques (->> (loom.alg/maximal-cliques graph)
                        (filter #(> (count %) 1)))
        all-nodes (apply clojure.set/union all-cliques)]
    (map #(assoc {}
            (identity %)
            (if (contains? all-nodes (identity %)) 1 0))
         (ug/nodes graph))))


(defn nodes-graph-metrics
  "associates graph metrics to nodes, for feature engineering
  input: ubergraph
  output: map of nodes and metrics {:id :nodeID :in-degree indegree ...}"
  [graph]
  (let [deg-centralities (apply merge (get-node-degree-centralities graph))
        betw-centralities (apply merge (get-node-betweenness-centralities graph))
        clos-centralities (apply merge (get-node-closeness-centralities graph))
        prank-centralities (apply merge (get-node-pagerank-centralities graph))
        weak-components-members (apply merge (get-weak-components-nodes graph))
        clique-nodes (apply merge (clique-belonging-nodes graph))]
    (map #(assoc {}
                 :id (identity %)
                 :in-degree (first (% deg-centralities))
                 :out-degree (second (% deg-centralities))
                 :betweenness (% betw-centralities)
                 :closeness (% clos-centralities)
                 :pagerank (% prank-centralities)
                 :in-cluster (% weak-components-members)
                 :in-cliques (% clique-nodes))
              (ug/nodes graph))))





(defn classify-graph-nodes
  "move this somewhere..? "
  [graph])