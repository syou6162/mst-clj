(ns mst-clj.minibatch
  (:use [mst-clj.io :only (my-pmap)])
  (:use [mst-clj.eisner :only (eisner-for-training)])
  (:use [mst-clj.perceptron :only (update-weight add-weight)])
  (:import [mst_clj.word Word])
  (:import [mst_clj.sentence Sentence]))

(def ^:dynamic *number-of-mini-batches* 24)
(def ^:dynamic *number-of-threads* 4)

(defn minibatch-update-weight [init-weight init-cum-weight gold-sentences]
  (let [n (count gold-sentences)
        mini-batches (->> gold-sentences
                          (partition-all *number-of-mini-batches*)
                          (vec))]
    (->> mini-batches
         (reduce
          (fn [[old-weight cum-old-weight] Dj]
            (let [C (->> Dj
                         (my-pmap
                          *number-of-threads*
                          (fn [gold] [gold (eisner-for-training gold old-weight)]))
                         (remove
                          (fn [[gold prediction]]
                            (= (map #(:head ^Word %) (:words gold))
                               (map #(:head ^Word %) (:words prediction))))))
                  result (reduce
                          (fn [[weight cum-weight] [gold prediction]]
                            (let [new-weight (update-weight weight gold prediction)
                                  new-cum-weight (add-weight cum-weight new-weight)]
                              [new-weight new-cum-weight]))
                          [old-weight cum-old-weight] C)]
              result))
          [init-weight init-cum-weight]))))
