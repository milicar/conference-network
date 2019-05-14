(ns conference-network.handler
  (:require [compojure.core :refer [defroutes routes]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.file-info :refer [wrap-file-info]]
            [hiccup.middleware :refer [wrap-base-url]]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [conference-network.web.routes.home :refer [home-routes]]
            [conference-network.web.views.pages :as p]
            [conference-network.ml.decision-tree :as dtree]
            [noir.session :as session]))

(defn init []
  (println "conference-network is starting")
  ;(def tree (dtree/initialize-tree dtree/data))
  )

(defn destroy []
  (println "conference-network is shutting down"))

(defroutes app-routes
  (route/resources "/")
  (route/not-found (p/not-found)))

(def app
  (-> (routes home-routes app-routes)
      (handler/site)
      (wrap-base-url)
      (session/wrap-noir-flash)
      (session/wrap-noir-session)))
