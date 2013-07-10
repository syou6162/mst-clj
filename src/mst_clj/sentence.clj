(ns mst-clj.sentence
  (:use [mst-clj.feature :only (*update-feature-id?* get-fv)]))

(defrecord Sentence [words edge-fvs])

(defn make [words]
  (let [n (count words)
        init-edge-fvs (vec (repeat n (vec (repeat n (int-array 0)))))
        edge-fvs (reduce (fn [result [i j]]
                           (binding [*update-feature-id?* false]
                             (assoc-in result [i j] (get-fv words i j))))
                         init-edge-fvs
                         (for [i (range n), j (range n) :when (not= i j)]
                           [i j]))]
    (Sentence. words edge-fvs)))
