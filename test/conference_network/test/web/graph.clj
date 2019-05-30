(ns conference-network.test.web.graph
  (:require [midje.sweet :refer :all]
            [conference-network.web.models.graph :as g]))


(facts "adding attributes to graph:"
       (fact "adding attributes merges new attributes with existing ones"
             (let [graph       (g/make-graph {:nodes {:node1 {:name "node1"} :node2 {:name "node2"}}})
                   nodes-attrs '({:node1 {:rank 1}} {:node2 {:rank 2}})]
               (g/add-nodes-attributes graph nodes-attrs) =>
               (contains {:attrs {:node1 {:name "node1" :rank 1} :node2 {:name "node2" :rank 2}}})))
       (fact "if attribute already existed, it is updated"
             (let [graph       (g/make-graph {:nodes {:node1 {:name "node1"} :node2 {:name "node2"}}})
                   nodes-attrs '({:node1 {:name "newname1"}} {:node2 {:name "newname2" :rank 2}})]
               (g/add-nodes-attributes graph nodes-attrs) =>
               (contains {:attrs {:node1 {:name "newname1"} :node2 {:name "newname2" :rank 2}}})))
       (fact "attempting to add attribute to non-existing nodes propagates ubergraph's exception"
             (let [graph       (g/make-graph {:nodes {:node1 {:name "node1"} :node2 {:name "node2"}}})
                   nodes-attrs '({:node2 {:rank 2}}{:node3 {:rank 3}})]
               (g/add-nodes-attributes graph nodes-attrs) => (throws IllegalArgumentException))))
