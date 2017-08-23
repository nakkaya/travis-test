
(defproject ferret "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [fast-zip "0.7.0"]
                 [org.clojure/tools.cli "0.3.3"]
                 [org.bituf/clj-stringtemplate "0.2"]
                 [org.clojars.amit/commons-io "1.4.0"]
                 [commons-lang "2.5"]
                 [watchtower "0.1.1"]
                 [org.clojure/tools.logging "0.2.3"]
                 [clojure-term-colors "0.1.0-SNAPSHOT"]]
  :repl-options {:host "0.0.0.0"
                 :port 7888
                 :init-ns ferret.core}

  :filespecs [{:type :bytes :path "build.info"
               :bytes ~(:out (clojure.java.shell/sh
                              "git" "rev-parse" "--short" "HEAD"))}]
  
  :main ferret.core
  :aot [ferret.core]
  :jar-name "interim.jar"
  :uberjar-name "ferret.jar")
