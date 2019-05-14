(ns conference-network.test.web.views.vega
  (:require [conference-network.web.views.vega :as v]
            [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [ubergraph.core :as u])
  (use [midje.util :only [testable-privates]]))

(testable-privates conference-network.web.views.vega extract-node-info extract-edges-info make-data)


(def empty-graph u/digraph)

(def isolates-graph
  (-> (u/digraph)
      (u/add-nodes-with-attrs* {:1 {:name "node1" :screen-name "@node1"} :2 {:name "node2" :screen-name "@node2"}})))

(def small-graph
  (-> (u/digraph)
      (u/add-nodes-with-attrs* {:1 {:name "node1" :screen-name "@node1"} :2 {:name "node2" :screen-name "@node2"}})
      (ubergraph.core/add-directed-edges [:1 :2 {:weight 5}])))



(facts "In the end, ids are left as keywords at extract-x-info"
       (extract-node-info isolates-graph) =>
       '({:id :1 :name "node1" :screen-name "@node1"} {:id :2 :name "node2" :screen-name "@node2"})

       (extract-node-info small-graph) =>
       '({:id :1 :name "node1" :screen-name "@node1"} {:id :2 :name "node2" :screen-name "@node2"})

       (extract-edges-info small-graph) => '({:source :1 :target :2 :weight 5})
       (extract-edges-info isolates-graph) => '())


(facts "Make data adds indexes to extracted node and edge info; this is until I figure out how
       to make vega data transformations work... in practice"
       (make-data isolates-graph) => [{:name "node-data", :values [{:name "node1", :screen-name "@node1", :id :1, :vega-id 0}
                                                                   {:name "node2", :screen-name "@node2", :id :2, :vega-id 1}]}
                                      {:name "link-data", :values []}]
       (make-data small-graph) => [{:name   "node-data",
                                    :values [{:name "node1", :screen-name "@node1", :id :1, :vega-id 0}
                                             {:name "node2", :screen-name "@node2", :id :2, :vega-id 1}]}
                                   {:name "link-data", :values [{:weight 5, :source 0, :target 1}]}])




