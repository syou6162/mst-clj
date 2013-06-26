(ns mst-clj.mapping
  (:use [clj-utils.io :only (serialize deserialize)]))
;; Reference: http://concurrent-trees.googlecode.com/svn/concurrent-trees/javadoc/apidocs/com/googlecode/concurrenttrees/radix/ConcurrentRadixTree.html

(import [com.googlecode.concurrenttrees.radix.node.concrete DefaultCharArrayNodeFactory])
(import [com.googlecode.concurrenttrees.radix ConcurrentRadixTree])

(defmacro def-obj-and-id-mapping [obj-name]
  "Define the functions related to converting object => id."
  (let [obj-to-id (symbol (str obj-name "-to-id"))
        id-to-obj (symbol (str "id-to-" obj-name))
        save-obj-to-id (symbol (str "save-" obj-name "-to-id"))
        load-obj-to-id (symbol (str "load-" obj-name "-to-id"))
        clear-obj-mapping! (symbol (str "clear-" obj-name "-mapping!"))
        get-max-obj-id (symbol (str "get-max-" obj-name "-id"))
        set-max-obj-id! (symbol (str "set-max-" obj-name "-id!"))]
    `(let [obj-to-id-mapping# (new ConcurrentRadixTree (new DefaultCharArrayNodeFactory))
           max-obj-id# (atom -1)]
       (defn ~get-max-obj-id [] @max-obj-id#)
       (defn ~set-max-obj-id! [id#] (reset! max-obj-id# id#))
       (defn ~obj-to-id [obj#]
         (if-let [result# (.getValueForExactKey obj-to-id-mapping# obj#)]
           result#
           (let [max-id# (swap! max-obj-id# inc)]
             (.put obj-to-id-mapping# obj# max-id#)
             max-id#)))
       (defn ~save-obj-to-id [filename#]
         (binding [*out* (java.io.FileWriter. filename#)]
           (println (~get-max-obj-id))
           (doseq [p# (-> obj-to-id-mapping#
                          (.getKeyValuePairsForKeysStartingWith  ""))]
             (let [k# (.getKey p#)
                   v# (.getValue p#)]
               (println (str k# " " v#))))))
       (defn ~load-obj-to-id [filename#]
         (with-open [br# (-> (java.io.FileInputStream. filename#)
                             (java.io.InputStreamReader.)
                             (java.io.BufferedReader.))]
           (~set-max-obj-id! (Integer/parseInt (. br# readLine)))
           (doseq [line# (line-seq br#)]
             (let [[k# v#] (clojure.string/split line# #" ")]
               (.put obj-to-id-mapping# k# (Integer/parseInt v#))))))
       (defn ~clear-obj-mapping! []
         (~set-max-obj-id! -1)
         (doseq [k# (-> obj-to-id-mapping#
                        (.getKeysStartingWith ""))]
           (.remove obj-to-id-mapping# k#))))))
