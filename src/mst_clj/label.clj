(ns mst-clj.label
  (:use [mst-clj.mapping :only (def-obj-and-id-mapping)])
  (:use [mst-clj.feature :only (get-fv feature-to-id)]))

(def-obj-and-id-mapping label)

(defn conjunctive-labeled-feature [label feature]
  (feature-to-id (str "label&" label \& feature)))

(defn get-labeled-fv
  ([label fv]
     (->> fv
          (map (partial conjunctive-labeled-feature label))
          (remove nil?)
          (int-array)))
  ([sentence i label j]
     (get-labeled-fv label (get-fv sentence i j))))
