(ns mst-clj.mapping
  (:use [clj-utils.io :only (serialize deserialize)]))

(import StringToIntPatriciaTrie)

(defmacro def-obj-and-id-mapping [obj-name]
  "Define the functions related to converting object <=> id."
  (let [obj-to-id (symbol (str obj-name "-to-id"))
        id-to-obj (symbol (str "id-to-" obj-name))
        save-obj-to-id (symbol (str "save-" obj-name "-to-id"))
        load-obj-to-id (symbol (str "load-" obj-name "-to-id"))
        clear-obj-mapping! (symbol (str "clear-" obj-name "-mapping!"))
        max-obj-id (symbol (str "max-" obj-name "-id"))]
    `(let [obj-to-id-mapping# (atom (StringToIntPatriciaTrie/factory))
           id-to-obj-mapping# (atom [])]
       (do
         (defn ~obj-to-id [obj#]
           (let [max-id# (count @obj-to-id-mapping#)]
             (if (.containsKey @obj-to-id-mapping# obj#)
               (.selectValue @obj-to-id-mapping# obj#)
               (do (.put @obj-to-id-mapping# obj# max-id#)
                   (swap! id-to-obj-mapping# conj obj#)
                   max-id#))))
         (defn ~id-to-obj [id#]
           (nth @id-to-obj-mapping# id#))
         (defn ~save-obj-to-id [filename#]
           (serialize {:obj-to-id-mapping @obj-to-id-mapping#
                       :id-to-obj-mapping @id-to-obj-mapping#}
                      filename#))
         (defn ~load-obj-to-id [filename#]
           (let [maps# (deserialize filename#)]
             (do
               (reset! obj-to-id-mapping# (:obj-to-id-mapping maps#))
               (reset! id-to-obj-mapping# (:id-to-obj-mapping maps#))
               nil)))
         (defn ~clear-obj-mapping! []
           (do (.clear @obj-to-id-mapping#)
               (reset! id-to-obj-mapping# [])
               nil))
         (defn ~max-obj-id []
           (.size @obj-to-id-mapping#))))))
