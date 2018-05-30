(ns conference-network.routes.home
  (:require [compojure.core :refer :all]
            [conference-network.views.pages :as pages]))

(defn home []
  (pages/home))

(defn about []
  (pages/about))

(defroutes home-routes
  (GET "/" [] (home))
  (GET "/about" [] (about)))
