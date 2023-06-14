(ns pichunter-files.core
  (:require [reitit.ring :as ring]
            [hugsql.core :refer [def-db-fns]]
            [clojure.java.io :as io]
            [reitit.ring.middleware.muuntaja :as reitit.middleware.muuntaja]
            [ring.adapter.jetty :refer [run-jetty]]
            [reitit.ring.middleware.parameters :as reitit.middleware.params]
            [reitit.ring.middleware.multipart :as middleware.multipart]
            [reitit.swagger-ui]
            [reitit.swagger]
            [mount.core :as mount :refer [defstate]]
            [muuntaja.core :as muuntaja]
            [reitit.coercion.spec]
            [reitit.ring.coercion :as ring.coercion]
            [hugsql.core :as hug]
            [hugsql.adapter.clojure-java-jdbc :as ad]
            [ring.middleware.session :as session])
  (:import [org.apache.commons.io IOUtils]
           [java.util UUID])
  (:gen-class))

(defstate db 
  :start (do
           (hug/set-adapter! (ad/hugsql-adapter-clojure-java-jdbc))
            {:user "pichunter"
             :password "TESTIPASSU"
             :host "localhost"
             :port "5432"
             :db "pichunter"
             :dbname "pichunter"
             :classname "org.postgresql.Driver"
             :dbtype "postgresql"})
  :stop nil)

(def-db-fns "sql/media.sql")


(defn wrap-db [handler]
  (fn [request]
    (handler (assoc request :db db))))

(defn post-pictures [{:keys [db]
                      {{:keys [file] :as mp} :multipart} :parameters}]
  (let [{:keys [filename tempfile content-type]} file
        bytedata (IOUtils/toByteArray (io/input-stream tempfile))]
    (first (insert-media db {:name filename
                             :data bytedata
                             :mime content-type}))))

(defn delete-pictures [{:keys [db]
                        {:keys [ids]} :body-params}]
  (let [ids (map #(UUID/fromString %) ids)]
    (doseq [id ids]
      (delete-picture* db {:id id}))))

(defn get-pictures [{:keys [db]
                     {{:keys [id]} :path} :parameters}]
  (let [{:keys [data name mime]} (get-media db {:id id})]
    {:status 200
     :body data
     :headers {"Content-Disposition" (str "inline; filename=" name)
               "Content-Type" mime}}))
  

(def app-routes [["/api" {:middleware [wrap-db]}
                  ["/pictures" {:swagger {:tags ["media"]}
                                ;; TODO maybe use a authentication middleware?
                                :middleware [middleware.multipart/multipart-middleware]
                                :post {:handler #'post-pictures
                                       :parameters {:multipart {:file middleware.multipart/temp-file-part}}}
                                :delete {:handler #'delete-pictures
                                         :parameters {:body {:ids [string?]}}}}]
                  ["/pictures"
                   ["/:id" {:swagger {:tags ["media"]}
                            :get {:handler #'get-pictures
                                  :parameters {:path {:id string?}}}}]]]
                 ["" {:no-doc true}
                  ["/swagger.json" {:get (reitit.swagger/create-swagger-handler)}]
                  ["/swagger/*" {:get (reitit.swagger-ui/create-swagger-ui-handler)}]]])

(defn router [options]
  (ring/router
   app-routes
   {:data {:coercion reitit.coercion.spec/coercion
           :muuntaja (-> muuntaja/default-options
                         muuntaja/create)
           :middleware [reitit.middleware.params/parameters-middleware
                        reitit.middleware.muuntaja/format-middleware
                        ring.coercion/coerce-exceptions-middleware
                        ring.coercion/coerce-request-middleware
                        ring.coercion/coerce-response-middleware]}}))

(defn app [options]
  (ring/ring-handler (router options)
                     (ring/create-default-handler)
                     {:middleware [session/wrap-session]}))


(defstate http-server
  :start (run-jetty (app {}) {:port 3001
                              :join? false
                              :host "127.0.0.1"})
  :stop (.stop http-server))

(defn go []
  (mount/start))

(defn stop []
  (mount/stop))

(defn reset []
  (stop)
  (go))

(defn -main [& _]
  (go)
  (println "pichunter file server running!"))
