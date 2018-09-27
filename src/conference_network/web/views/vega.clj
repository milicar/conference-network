(ns conference-network.web.views.vega
  (:require [ubergraph.core :as g]
            [clojure.data.json :as json]))


(defn- id->vega-id
  "get vega-id of a node"
  [id indexed-nodes]
  (-> (filter #(= id (:id %)) indexed-nodes)
      (first)
      :vega-id))

(defn- vegaize-edges
  "replace ubergraph nodes ids with vega nodes ids
  bc I don't know how to make vega work with joins"
  [edges indexed-nodes]
  (map #(assoc %
          :source (id->vega-id (:source %) indexed-nodes)
          :target (id->vega-id (:target %) indexed-nodes))
       edges))

(defn- extract-edges-info
  "extracts egdes info and attributes
  input: graph
  output: map of edges {&attrs :src src :dest dest}"
  [graph]
  (map #(assoc (g/attrs graph %) :source (:src %) :target (:dest %)) (g/edges graph)))

(defn- add-vega-index
  "adds index to nodes to help me with vega"
  [nodes]
  (map #(assoc %1 :vega-id %2) nodes (range (count nodes))))

(defn- extract-node-info
  "extracts node info and attributes"
  [graph]
  (map #(assoc (g/attrs graph %) :id (identity %)) (g/nodes graph)))

(defn- make-data
  "builds nodes and edges data for vega spec
  input: graph
  output: vector of two maps, each containing a name and a vector of values, which are maps"
  [graph]
  (let [nodes (extract-node-info graph)
        indexed-nodes (add-vega-index nodes)
        edges (extract-edges-info graph)
        vega-edges (vegaize-edges edges indexed-nodes)]
    [{:name "node-data"
      :values (vec indexed-nodes)}
     {:name "link-data"
      :values (vec vega-edges)}]))

(defn- make-spec
  "makes vega specification, very concrete"
  [vega-data]
  (let [width 700
        height 500
        node-radius 8
        node-charge -15
        link-distance 15]
  {
   :$schema  "https://vega.github.io/schema/vega/v4.json",
   :autosize "none",
   :width    width,
   :height   height,
   :padding  0,

   :signals  [{:name "cx", :update "width / 2"}
              {:name "cy", :update "height / 2"}
              {:name "nodeRadius", :value node-radius}
              {:name "nodeCharge", :value node-charge}
              {:name "linkDistance", :value link-distance}
              {:name "static", :value true}
              {:description "State variable for active node fix status.",
               :name        "fix",
               :value       0,
               :on          [{:events "symbol:mouseout[!event.buttons], window:mouseup", :update "0"}
                             {:events "symbol:mouseover", :update "fix || 1"}
                             {:events "[symbol:mousedown, window:mouseup] > window:mousemove!", :update "2", :force true}]}
              {:description "Graph node most recently interacted with.",
               :name        "node",
               :value       nil,
               :on          [{:events "symbol:mouseover", :update "fix === 1 ? item() : node"}]}
              {:description "Flag to restart Force simulation upon data changes.",
               :name        "restart",
               :value       false,
               :on          [{:events {:signal "fix"}, :update "fix > 1"}]}],

   :scales   [{:name "color", :type "ordinal", :range {:scheme "category10"}}],

   :marks    [{:name      "nodes",
               :type      "symbol",
               :zindex    1,
               :from      {:data "node-data"},
               :on        [{:trigger "fix", :modify "node", :values "fix === 1 ? {fx:node.x, fy:node.y} : {fx:x(), fy:y()}"}
                           {:trigger "!fix", :modify "node", :values "{fx: null, fy: null}"}],
               :encode    {:enter  {:fill {:scale "color", :field "group"}, :stroke {:value "white"}},
                           :update {:size {:signal "2 * nodeRadius * nodeRadius"}, :cursor {:value "pointer"}}},
               :transform [{:type       "force",
                            :iterations 300,
                            :restart    {:signal "restart"},
                            :static     {:signal "static"},
                            :forces     [{:force "center", :x {:signal "cx"}, :y {:signal "cy"}}
                                         {:force "collide", :radius {:signal "nodeRadius"}}
                                         {:force "nbody", :strength {:signal "nodeCharge"}}
                                         {:force "link", :links "link-data", :distance {:signal "linkDistance"}}]}]}
              {:type        "path",
               :from        {:data "link-data"},
               :interactive false,
               :encode      {:update {:stroke {:value "black"}, :strokeWidth {:value 1}}},
               :transform   [{:type    "linkpath",
                              :shape   "line",
                              :sourceX "datum.source.x",
                              :sourceY "datum.source.y",
                              :targetX "datum.target.x",
                              :targetY "datum.target.y"}]}],

   :data vega-data}))

(defn- write-json
  "makes json from clj data"
  [spec-data]
  (json/write-str spec-data :escape-slash false))

(defn make-vega-spec
  "makes vega specification for the graph
  input: ubergraph
  output: json"
  [graph]
  (-> graph
      (make-data)
      (make-spec)
      (write-json)))
