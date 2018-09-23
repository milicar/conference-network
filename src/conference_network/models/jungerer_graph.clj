(ns conference-network.models.jungerer-graph
  (:require [jungerer.graph :as jg]
            [jungerer.algo :as alg]
            [ubergraph.core :as ug])
  (:import [edu.uci.ics.jung.algorithms.cluster BicomponentClusterer EdgeBetweennessClusterer WeakComponentClusterer]))


;; it appears jungerer doesn't support edge weights or any other
;; attribute of the edges;
;; saving a graph saves only edges, so isolates are lost in reloaded graph
;; but it has implementation of functions such as nodes centralities,
;; which are not implemented in ubergraph, so jungerer will be used for
;; those calculations


(defn parse-uber-edges
  "parses edges form ubergraph edge structure to vectors of pairs of nodes
  input: ubergraph
  output: [[1 2] [2 3] ..]"
  [ubergraph]
  (map #(vector (:src %) (:dest %)) (ug/edges ubergraph)))


(defn make-jung-from-ubergraph
  "makes a jungerer directed graph from an ubergraph directed graph;
  isolates are excluded, as they should be for distance based calculations
  input: ubergraph directed graph
  output: jungerer directed graph"
  [ubergraph]
  (let [edges (parse-uber-edges ubergraph)]
        (jg/directed-graph edges)))


(def ubergraph->jung (memoize make-jung-from-ubergraph))


(def centralities {:betweenness (partial alg/betweenness-centrality)
                   :closeness (partial alg/closeness-centrality)
                   :eigenvector (partial alg/eigenvector-centrality)
                   :page-rank (partial alg/page-rank)})

(defn get-centralities
  "converts ubergraph to jungerer graph and finds centrality scores for each node
  input: ubergraph, kind of centrality
  output: sequence of maps {:node centrality-score}"
  [ubergraph kind]
  (let [jungraph (ubergraph->jung ubergraph)
        scorer ((kind centralities) jungraph)]
    (map #(assoc {} % (alg/score scorer %))
         (jungerer.graph/nodes jungraph))))


(defn betweenness-centralities
  [ubergraph]
  (get-centralities ubergraph :betweenness))

(defn closeness-centralities
  [ubergraph]
  (get-centralities ubergraph :closeness))

(defn eigenvector-centralities
  [ubergraph]
  (get-centralities ubergraph :eigenvector))

(defn pagerank-centralities
  [ubergraph]
  (get-centralities ubergraph :page-rank))


(defn extract-weak-components
  [jungerer-graph]
  (.apply (WeakComponentClusterer.) jungerer-graph))


(defn edge-betweenness-clusters
  [jungerer-graph edges-to-remove]
  (.apply (EdgeBetweennessClusterer. edges-to-remove) jungerer-graph))


(defn nodes-by-weak-components
  "converts ubergraph to jungerer graph and finds weak components; then for each
   node from original graph (so to include isolates as well), assigns 1 if it
   belongs to any component, 0 if not
   input: ubergraph
   output: sequence of maps: {:node 0/1}"
  [ubergraph]
  (let [jungerer-graph (ubergraph->jung ubergraph)
        weak-components (extract-weak-components jungerer-graph)
        nodes-in (flatten (map seq (filter #(> (count %) 1) weak-components)))
        nodes-out (clojure.set/difference (set (ug/nodes ubergraph)) (set nodes-in))]
    (concat (map #(assoc {} % 1) nodes-in)
            (map #(assoc {} % 0) nodes-out))))


(defn nodes-by-edge-betweenness-clusters
  "converts ubergraph to jungerer graph and makes clusters based on edge betweenness
   then for each node from original graph (so to include isolates as well), assigns
   1 if it belongs to any cluster, 0 if not
   input: ubergraph
   output: sequence of maps: {:node 0/1}"
  [ubergraph edges-to-remove]
  (let [jungerer-graph (ubergraph->jung ubergraph)
        eb-clusters (edge-betweenness-clusters jungerer-graph edges-to-remove)
        nodes-in (flatten (map seq (filter #(> (count %) 1) eb-clusters)))
        nodes-out (clojure.set/difference (set (ug/nodes ubergraph)) (set nodes-in))]
    (concat (map #(assoc {} % 1) nodes-in)
            (map #(assoc {} % 0) nodes-out))))




; only for undirected graphs!
;(defn bicomponents-clusters
;  [jungerer-graph]
;  (.apply (BicomponentClusterer.) jungerer-graph))


;;;;; in case I change my mind and switch to jungerer only
;; graph elements that make-graph is called on are in the form:
;; {{:nodes {:tw-id {:name "name" :screen-name "scr-n"}, :tw-id2 {....}}}
;;  {:edges {:id-src {:id-dest weight/count :id-dest2 weight} :id-src2.... }}}
;
;(defn add-nodes
;  [graph nodes-map]
;  (map #(g/add-node! graph %) (keys nodes-map)))
;
;(defn parse-edges-for-source
;  "parses edges for one source from format {:source {:dest1 weight1, :dest2 weight2}} to
;  [[:source :dest1][:source :dest2]]"
;  [edges]
;  (reduce #(conj %1 [(first edges) (first %2)]) [] (second edges)))
;
;(def add-edges
;  [graph edges-map]
;  (->> edges-map
;      (map parse-edges-for-source)
;      (apply concat)
;       (map #(g/add-edge! graph %))))
;
;(defn make-graph
;  "main function, called from outside with graph elements
;  input: graph element (see above)
;  output: jungerer directed-graph"
;  [elements]
;  (let [graph (g/directed-graph)]
;    (-> graph
;        (add-nodes (:nodes elements))
;        (add-edges (:edges elements)))))
;
