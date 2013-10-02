(ns mst-clj.sentence
  (:use [mst-clj.label :only (get-labeled-fv label-to-id-mapping)])
  (:use [mst-clj.feature :only (*update-feature-id?* get-fv)]))

(defrecord Sentence [words edge-fvs])

(defn make [words]
  (let [n (count words)
        init-edge-fvs {}
        labels (set (map first (label-to-id-mapping)))
        edge-fvs (binding [*update-feature-id?* false]
                   (->> (for [i (range n), j (range n)
                              :when (not= i j)
                              :let [fv (get-fv words i j)]
                              label labels]
                          [i label j fv])
                        (reduce (fn [result [i label j fv]]
                                  (assoc-in result [i label j]
                                            (get-labeled-fv label fv)))
                                init-edge-fvs)))]
    (Sentence. words edge-fvs)))
