(ns conference-network.test.ml.feature-engineering
  (:require [midje.sweet :refer :all]
            [conference-network.ml.feature-engineering :as fe]))


(fact "get-raw-features combines graph and tweets features into one 'matrix' of observations"

      (fact
        (fe/get-raw-features anything anything) => '({:id          :540281954, :in-degree 0.0, :out-degree 0.005319148936170213,
                                                      :betweenness 0.0, :closeness 1.0, :pagerank 0.002242496714584356,
                                                      :in-cluster  1, :in-cliques 0, :num-tweets 5}
                                                      {:id         :174244919, :in-degree 0.06914893617021277,
                                                       :out-degree 0.02659574468085106, :betweenness 483.16666666666663,
                                                       :closeness  0.29523809523809524, :pagerank 0.017721069947866996,
                                                       :in-cluster 1, :in-cliques 1, :num-tweets 1}
                                                      {:id          :846249998996123648, :in-degree 0.0, :out-degree 0.005319148936170213,
                                                       :betweenness 0.0, :closeness 0.6, :pagerank 0.002242496714584356,
                                                       :in-cluster  1, :in-cliques 1, :num-tweets 3}))

      (against-background (conference-network.web.models.graph/nodes-graph-metrics anything) =>
                          '({:id          :540281954, :in-degree 0.0, :out-degree 0.005319148936170213,
                             :betweenness 0.0, :closeness 1.0, :pagerank 0.002242496714584356,
                             :in-cluster  1, :in-cliques 0}
                             {:id         :174244919, :in-degree 0.06914893617021277,
                              :out-degree 0.02659574468085106, :betweenness 483.16666666666663,
                              :closeness  0.29523809523809524, :pagerank 0.017721069947866996,
                              :in-cluster 1, :in-cliques 1}
                             {:id          :846249998996123648, :in-degree 0.0, :out-degree 0.005319148936170213,
                              :betweenness 0.0, :closeness 0.6, :pagerank 0.002242496714584356,
                              :in-cluster  1, :in-cliques 1})
                          (conference-network.web.models.tweets/number-of-tweets-per-user anything) =>
                          {:540281954 5 :174244919 1 :846249998996123648 3}))


(fact "deal-with-nils-and-nans encodes several possible cases of nil/NaN occurences in calculated data"
      (fact "if all's well, do nothing"
            (let [raw-features '({:id          :701475135287664641, :in-degree 0.0, :out-degree 0.01063829787234043,
                                  :betweenness 0.0, :closeness 0.3387096774193548,
                                  :pagerank    0.002242496714584356, :in-cluster 1, :in-cliques 0,
                                  :num-tweets  1}
                                  {:id          :310976398, :in-degree 0.0, :out-degree 0.01063829787234043,
                                   :betweenness 0.0, :closeness 0.3088235294117647,
                                   :pagerank    0.002242496714584356, :in-cluster 1,
                                   :in-cliques  0, :num-tweets 1})]
              (fe/deal-with-nils-and-nans raw-features) => raw-features))
      (fact "replace nil betweenness, closeness and pagerank centralities with -1.0,
             nil counts with 0, NaN closeness centrality with -0.5"
            (let [raw-features '({:id          :701475135287664641, :in-degree 0.0, :out-degree 0.01063829787234043,
                                  :betweenness nil, :closeness nil,
                                  :pagerank    nil, :in-cluster 1, :in-cliques 0,
                                  :num-tweets  nil}
                                  {:id          :310976398, :in-degree 0.0, :out-degree 0.01063829787234043,
                                   :betweenness 0.0, :closeness 0.3088235294117647,
                                   :pagerank    0.002242496714584356, :in-cluster 1,
                                   :in-cliques  0, :num-tweets 1})]
              (fe/deal-with-nils-and-nans raw-features) =>
              '({:id          :701475135287664641, :in-degree 0.0, :out-degree 0.01063829787234043,
                 :betweenness -1.0, :closeness -1.0, :pagerank    -1.0, :in-cluster 1, :in-cliques 0,
                 :num-tweets  0}
                 {:id          :310976398, :in-degree 0.0, :out-degree 0.01063829787234043,
                  :betweenness 0.0, :closeness 0.3088235294117647, :pagerank    0.002242496714584356,
                  :in-cluster 1, :in-cliques  0, :num-tweets 1})))
      (fact "doesn't encode anything else"
            (let [raw-features '({:id          nil, :in-degree nil, :out-degree nil,
                                  :betweenness 0.0, :closeness 0.3387096774193548,
                                  :pagerank    0.002242496714584356, :in-cluster nil, :in-cliques nil,
                                  :num-tweets  1})]
              (fe/deal-with-nils-and-nans raw-features) => raw-features)))






