(ns conference-network.test.handler
  (:use midje.sweet
        clojure.test
        ring.mock.request
        conference-network.handler))

(deftest test-app
  (testing "main route"
    (let [response (app (request :get "/"))]
      (is (= (:status response) 200))
      (is (.contains (:body response) "Home again"))))

  (testing "not-found route"
    (let [response (app (request :get "/invalid"))]
      (is (= (:status response) 404))
      (is (.contains (:body response) "Ooops!"))))
  )

(facts "about routing"
       (fact "main route responds with status ok and page that contains Visualizing conference network header"
             (let [response (app (request :get "/"))]
               (:status response) => 200
               (.contains (:body response) "<h1>Visualizing conference network</h1>") => true))
       (fact "not-found route responds with status 404 and page that contains Ooops!"
             (let [response (app (request :get "/invalid"))]
               (:status response) => 404
               (.contains (:body response) "Ooops!") => true))
       (fact "about route responds with status ok and page that contains About header"
             (let [response (app (request :get "/about"))]
               (:status response) => 200
               (.contains (:body response) "<h1>About</h1>") => true)))


