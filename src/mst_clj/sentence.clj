(ns mst-clj.sentence
  (:use [mst-clj.label :only (get-labeled-fv label-to-id-mapping)])
  (:use [mst-clj.feature :only (*update-feature-id?* get-fv)]))

(defrecord Sentence [words edge-fvs])

(defn make [words]
  (let [n (count words)
        init-edge-fvs {}
        edge-fvs (reduce (fn [result [i j]]
                           (binding [*update-feature-id?* false]
                             (let [fv (get-fv words i j)]
                               (->> (label-to-id-mapping)
                                    (reduce
                                     (fn [result [label id]]
                                       (assoc-in result [i label j] (get-labeled-fv label fv)))
                                     result)))))
                         init-edge-fvs
                         (for [i (range n), j (range n) :when (not= i j)]
                           [i j]))]
    (Sentence. words edge-fvs)))
