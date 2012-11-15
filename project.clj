(defproject mst-clj "0.0.1"
  :description "MST parser"
  :url "https://github.com/syou6162/mst-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [info.yasuhisay/clj-utils "0.1.1"]
                 [org.clojure/tools.cli "0.2.1"]
                 [org.ardverk/patricia-trie "0.7-SNAPSHOT"]
                 [org.clojure/math.combinatorics "0.0.3"]]
  :jvm-opts ["-Xmx8G" "-server" "-Dfile.encoding=UTF-8"]
  :java-source-paths ["src/java/"]
  :repositories [["org.ardverk" "http://mvn.ardverk.org/repository/snapshot"]]
  :main mst-clj.core)
