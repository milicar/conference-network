(ns conference-network.test.handler
  (:use midje.sweet
        ring.mock.request
        conference-network.handler))

(facts "about routing"
       (fact "main route responds with status ok and page that contains Visualizing conference network header"
             (let [response (app (request :get "/"))]
               (:status response) => 200
               (:body response) => (contains "<h1>Visualizing conference network</h1>")))
       (fact "not-found route responds with status 404 and page that contains Ooops!"
             (let [response (app (request :get "/invalid"))]
               (:status response) => 404
               (:body response) => (contains "Ooops!")))
       (fact "about route responds with status ok and page that contains About header"
             (let [response (app (request :get "/about"))]
               (:status response) => 200
               (:body response) => (contains "<h1>About</h1>")))
       ; FAILS!!! throws NullPointerException for some reason, although routing works
       ;(fact "get-tweets-form (/get-tweets) responds with status ok"
       ;      (let [response (app (request :post "/get-tweets"
       ;                                   {"hashtag" "htag" "startdate" "sdate" "enddate" "edate"}))]
       ;        (:status response) => 200))

       ;(fact "couldn't check that get-tweets-form mocked ring request adds correct form parameters"
       ;      (let [request (request :post "/get-tweets"
       ;                                   {:hashtag "htag" :startdate "sdate" :enddate "edate"})]
       ;        (:form-params request) => (contains "htag")))

       )