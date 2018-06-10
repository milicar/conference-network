(ns conference-network.models.db
  (:require [clojure.java.jdbc :as db]))

(def mysql-db (read-string (slurp "db-config.txt")))



(defn create-account
  "creates a user account"
  [user pass]
  (db/insert! mysql-db :user {:username user :password pass}))

(defn get-user
  "retrieves user from database"
  [user]
  (db/query mysql-db ["SELECT * FROM user WHERE username = ?" user]))

(defn get-user-id
  "retrieves user id for a username (username is also unique)"
  [user]
  (-> (db/query mysql-db ["SELECT user_id FROM user WHERE username = ?" user])
      first
      :user_id))

(defn check-user
  "checks if there is a user with specified credentials"
  [user pass]
  (db/query mysql-db ["SELECT * FROM user WHERE username = ? AND password = ?" user pass]))

(defn update-password
  "updates a user account"
  [user pass]
  (db/update! mysql-db :user {:password pass} ["username = ?" user]))

(defn delete-user
  "deletes a user from database"
  [user]
  (db/delete! mysql-db :user ["username = ?" user]))

(defn save-graph
  "saves a graph for a user"
  [user graph]
  (let [user-id (get-user-id user)]
    (db/insert! mysql-db :network_graph {:user_id user-id :graph graph})))

(defn get-graphs
  "retrieves all the graphs for a user"
  [user]
  (let [user-id (get-user-id user)]
    (db/query mysql-db ["SELECT graph FROM network_graph WHERE user_id = ?" user-id])))

(defn get-graph-id
  "retrieves id of a graph"
  [graph]
  (-> (db/query mysql-db ["SELECT graph_id FROM network_graph WHERE graph = ?" graph])
      first
      :graph_id))

(defn get-graph-by-id
  "retrieves a graph based on id"
  [graph-id]
  (db/query mysql-db ["SELECT * FROM network_graph WHERE graph_id = ?" graph-id]))

(defn update-graph
  "updates a graph"
  [graph-id new-value]
  (db/update! mysql-db :network_graph {:graph new-value} ["graph_id = ?" graph-id]))

(defn delete-graph
  "deletes a graph from database"
  [graph-id]
  (db/delete! mysql-db :network_graph ["graph_id = ?" graph-id]))