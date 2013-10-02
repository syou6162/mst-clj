(ns mst-clj.label
  (:use [mst-clj.mapping :only (def-obj-and-id-mapping)])
  (:use [mst-clj.feature :only (get-fv feature-to-id)])
  (:require [clojure.core.reducers :as r]))

(def-obj-and-id-mapping label)

(defn conjunctive-labeled-feature [label feature]
  (feature-to-id (str "label&" label \& feature)))

(defn get-labeled-fv
  ([label fv]
     (->> fv
          (r/map (partial conjunctive-labeled-feature label))
          (r/remove nil?)
          (r/fold (r/monoid into vector) conj)
          (into fv)
          (int-array)))
  ([sentence i label j]
     (get-labeled-fv label (get-fv sentence i j))))
