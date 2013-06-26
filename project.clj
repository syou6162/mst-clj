(defproject mst-clj "0.0.1"
  :description "MST parser"
  :url "https://github.com/syou6162/mst-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [info.yasuhisay/clj-utils "0.1.1"]
                 [org.clojure/tools.cli "0.2.1"]
                 [com.googlecode.concurrent-trees/concurrent-trees "2.0.0"]
                 [org.clojure/math.combinatorics "0.0.3"]]
  :jvm-opts ["-Xmx8G" "-server" "-Dfile.encoding=UTF-8"]
  :main mst-clj.core)
