(ns mst-clj.minibatch
  (:use [mst-clj.io :only (my-pmap)])
  (:use [mst-clj.eisner :only (eisner-for-training)])
  (:use [mst-clj.perceptron :only (fv-diff get-step-size update-weight!)])
  (:import [mst_clj.word Word])
  (:import [mst_clj.sentence Sentence]))

(def ^:dynamic *number-of-mini-batches* 24)
(def ^:dynamic *number-of-threads* 4)

(defn minibatch-update-weight
  [iter ^doubles init-weight ^doubles init-cum-weight gold-sentences]
  (let [n (count gold-sentences)
        mini-batches (->> gold-sentences
                          (map-indexed #(vector (inc %1) %2))
                          (partition-all *number-of-mini-batches*)
                          (vec))]
    (->> mini-batches
         (reduce
          (fn [[old-weight cum-old-weight] Dj]
            (let [C (->> Dj
                         (my-pmap
                          *number-of-threads*
                          (fn [[idx gold]]
                            (let [prediction (eisner-for-training gold old-weight)]
                              [idx gold prediction])))
                         (remove
                          (fn [[idx gold prediction]]
                            (= (map #(:head ^Word %) (:words gold))
                               (map #(:head ^Word %) (:words prediction))))))]
              (doseq [[idx gold prediction] C]
                (let [step-size (get-step-size old-weight gold prediction)
                      diff (->> (fv-diff gold prediction)
                                (map (fn [[k v]] [k (* step-size v)])))
                      number-of-cum-examples (+ (* iter n) idx)]
                  (update-weight! old-weight diff 1.0)
                  (update-weight! cum-old-weight diff number-of-cum-examples)))
              [old-weight cum-old-weight]))
          [init-weight init-cum-weight]))))
