(defproject kunstmusik/mapfs "0.1.0-SNAPSHOT"
  :description "Use Clojure map like a filesystem"
  :url "http://github.com/kunstmusik/mapfs"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [jline/jline "2.12"]]
 
  :aot [mapfs.core] 
  :main mapfs.core 
  )
