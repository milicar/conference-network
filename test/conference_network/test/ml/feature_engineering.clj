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

(facts "filter-out-nils filters any nil if needed; for unpredicted nils or in general.."
       (fact "filters out row where any value is nil"
             (let [raw-features '({:id          :310976398, :in-degree nil, :out-degree 0.01063829787234043,
                                   :betweenness 0.0, :closeness 0.3088235294117647,
                                   :pagerank    0.002242496714584356, :in-cluster 1,
                                   :in-cliques  0, :num-tweets 1})]
               (fe/filter-out-nils raw-features) => '())
             (let [raw-features '({:id :701475135287664641, :in-degree 0.0, :out-degree 0.01063829787234043,
                                     :betweenness -1.0, :closeness -1.0, :pagerank -1.0, :in-cluster 1, :in-cliques 0,
                                     :num-tweets  0}
                                     {:id nil, :in-degree 0.0, :out-degree 0.01063829787234043,
                                      :betweenness 0.0, :closeness 0.3088235294117647, :pagerank 0.002242496714584356,
                                      :in-cluster 1, :in-cliques  0, :num-tweets 1})]
               (fe/filter-out-nils raw-features) => '({:id :701475135287664641, :in-degree 0.0, :out-degree 0.01063829787234043,
                                                       :betweenness -1.0, :closeness -1.0, :pagerank -1.0, :in-cluster 1, :in-cliques 0,
                                                       :num-tweets  0}))))

(facts "rescale-feature takes all the values of a particular feature in a dataset and converts them
        into a fraction"
       (fact "counts (longs) become doubles"
             (let [feature :num-tweets
                   rows '({:num-tweets 0}{:num-tweets 52}{:num-tweets 26})]
               (fe/rescale-feature feature rows) => '({:num-tweets 0.0}{:num-tweets 1.0}{:num-tweets 0.5})))
       (fact "nils are left as they are"
             (let [feature :num-tweets
                   rows '({:num-tweets 0}{:num-tweets nil}{:num-tweets 26})]
               (fe/rescale-feature feature rows) => '({:num-tweets 0.0}{:num-tweets nil}{:num-tweets 1.0})))
       (fact "doubles are left doubles"
             (let [feature :betweenness
                   rows '({:betweenness 912.1333333333333}{:betweenness 214.91666666666669}{:betweenness 352.0})]
               (fe/rescale-feature feature rows) => '({:betweenness 1.0}{:betweenness 0.23561979242800762}
                                                       {:betweenness 0.3859084929103932})))
       (fact "negatives are introduced to signify something is very off with some nodes, eg they are isolates
              rescaling -1.0 or -2.0 to easily something in the order of 10e-3 or less would diminish the
              significance of the encoding, so negatives are left as they are "
             (let [feature :betweenness
                   rows '({:betweenness 912.13}{:betweenness -1.0}{:betweenness -2.0}{:betweenness 352.0})]
               (fe/rescale-feature feature rows) => '({:betweenness 1.0}{:betweenness -1.0}{:betweenness -2.0}
                                                       {:betweenness 0.3859099031936237}))))


(facts "round-feature rounds feature values, once they are fractions"
       (fact "default is 2 decimal places"
             (let [feature :betweenness
                   rows    '({:betweenness 0.333333333333} {:betweenness 0.3849099031936237})]
               (fe/round-feature feature rows) => '({:betweenness 0.33}{:betweenness 0.38})))
       (fact "nils and negatives don't round"
             (let [feature :betweenness
                   rows    '({:betweenness -2.0} {:betweenness 0.3849099031936237} {:betweenness nil})]
               (fe/round-feature feature rows) => '({:betweenness -2.0}{:betweenness 0.38}{:betweenness nil})))
       (fact "providing a different number of decimal places"
             (let [feature :betweenness
                   rows    '({:betweenness 0.333333333333} {:betweenness 0.3849099031936237})]
               (fe/round-feature feature rows 3) => '({:betweenness 0.333} {:betweenness 0.385}))
             (let [feature :betweenness
                   rows    '({:betweenness 0.333333333333} {:betweenness 0.3849099031936237})]
               (fe/round-feature feature rows 1) => '({:betweenness 0.3} {:betweenness 0.4})))
       (fact "watch out for 0 deimal places.."
             (let [feature :betweenness
                   rows    '({:betweenness 0.333333333333} {:betweenness 0.3849099031936237})]
               (fe/round-feature feature rows 0) => '({:betweenness 0.0} {:betweenness 0.0}))))








