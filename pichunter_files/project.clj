(defproject pichunter_files "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [metosin/muuntaja "0.6.7"]
                 [mount "0.1.16"]
                 [hiccup "1.0.5"]
                 [org.postgresql/postgresql "42.2.18"]
                 [org.clojure/java.jdbc "0.7.11"]
                 [ring/ring-jetty-adapter "1.8.2"]
                 [metosin/reitit "0.5.10"]
                 [com.layerware/hugsql-core "0.5.1"]
                 [com.layerware/hugsql-adapter-clojure-java-jdbc "0.5.1"]]
  :repl-options {:init-ns pichunter-files.core})
