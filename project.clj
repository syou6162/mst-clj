(defproject mst-clj "0.0.3"
  :description "MST parser"
  :url "https://github.com/syou6162/mst-clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.codehaus.jsr166-mirror/jsr166y "1.7.0"]
                 [info.yasuhisay/clj-utils "0.1.1"]
                 [org.clojure/tools.cli "0.2.1"]]
  :aot [mst-clj.word mst-clj.sentence]
  :jvm-opts ["-Xms30G" "-Xmx30G" "-server"
             "-Dfile.encoding=UTF-8"
             "-XX:+UseConcMarkSweepGC"
             "-XX:+CMSParallelRemarkEnabled"
             "-XX:+UseParNewGC"]
  :main mst-clj.core)
