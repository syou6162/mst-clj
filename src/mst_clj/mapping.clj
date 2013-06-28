(ns mst-clj.mapping
  (:use [clj-utils.io :only (serialize deserialize)]))

(defmacro def-obj-and-id-mapping [obj-name]
  "Define the functions related to converting object => id."
  (let [obj-to-id (symbol (str obj-name "-to-id"))
        save-obj-to-id (symbol (str "save-" obj-name "-to-id"))
        load-obj-to-id (symbol (str "load-" obj-name "-to-id"))
        clear-obj-mapping! (symbol (str "clear-" obj-name "-mapping!"))
        obj-to-id-mapping (symbol (str obj-name "-to-id-mapping"))
        max-obj-id (symbol (str "max-" obj-name "-id"))]
    `(let [obj-to-id-mapping# (atom {})]
       (do
         (defn ~obj-to-id [obj#]
           (if-let [v# (get @obj-to-id-mapping# obj#)]
             v#
             (let [max-id# (count @obj-to-id-mapping#)]
               (swap! obj-to-id-mapping# assoc obj# max-id#)
               max-id#)))
         (defn ~save-obj-to-id [filename#]
           (serialize @obj-to-id-mapping# filename#))
         (defn ~load-obj-to-id [filename#]
           (reset! obj-to-id-mapping# (deserialize filename#)))
         (defn ~clear-obj-mapping! []
           (reset! obj-to-id-mapping# {}))
         (defn ~obj-to-id-mapping [] @obj-to-id-mapping#)
         (defn ~max-obj-id []
           (count @obj-to-id-mapping#))))))
