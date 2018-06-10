(defproject conference-network "0.1.0-SNAPSHOT"
  :description "visualize conference's network based on participants' tweets"
  :url "https://github.com/milicar/conference-network"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [compojure "1.5.2"]
                 [hiccup "1.0.5"]
                 [ring-server "0.4.0"]
                 [bouncer "1.0.1"]
                 [clojure.java-time "0.3.2"]
                 [twitter-api "1.8.0"]
                 [org.clojure/core.async "0.4.474"]
                 [ubergraph "0.5.0"]
                 [org.clojure/java.jdbc "0.7.6"]
                 [mysql/mysql-connector-java "5.1.38"]]
  :plugins [[lein-ring "0.8.12"]]
  :ring {:handler conference-network.handler/app
         :init conference-network.handler/init
         :destroy conference-network.handler/destroy}
  :profiles
  {:uberjar {:aot :all}
   :production
            {:ring
             {:open-browser? false, :stacktraces? false, :auto-reload? false}}
   :dev
            {:dependencies [[ring/ring-mock "0.3.2"]
                            [ring/ring-devel "1.5.1"]
                            [midje "1.9.1"]
                            [org.senatehouse/expect-call "0.1.0"]] }
   })
