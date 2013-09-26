(ns mst-clj.minibatch
  (:use [mst-clj.io :only (my-pmap)])
  (:use [mst-clj.eisner :only (eisner-for-training)])
  (:use [mst-clj.perceptron
         :only (get-fv-diff get-step-size update-weight! completely-correct?)])
  (:use [mst-clj.evaluation
         :only (get-dependency-accuracy get-labeled-dependency-accuracy
                get-complete-accuracy get-labeled-complete-accuracy)])
  (:import [mst_clj.word Word])
  (:import [mst_clj.sentence Sentence]))

(def ^:dynamic *number-of-mini-batches* 24)
(def ^:dynamic *number-of-threads* (min 4 *number-of-mini-batches*))

(defn get-violated-set [^doubles weight mini-batch]
  (let [result (->> mini-batch
                    (my-pmap
                     *number-of-threads*
                     (fn [[idx gold]]
                       (let [prediction (eisner-for-training weight gold)
                             fv-diff (get-fv-diff gold prediction)]
                         [idx gold prediction fv-diff]))))
        predictions (mapv #(nth % 2) result)]
    (-> (remove
         (fn [[_ gold predict _]] (completely-correct? gold predict))
         result)
        (with-meta {:predictions predictions}))))

(defn minibatch-update-weight
  [iter ^doubles weight ^doubles cum-weight gold-sentences]
  (let [predicts (atom [])
        n (count gold-sentences)
        mini-batches (->> gold-sentences
                          (map-indexed #(vector (inc %1) %2))
                          (partition-all *number-of-mini-batches*)
                          (vec))]
    (doseq [Dj mini-batches]
      (let [C (get-violated-set weight Dj)]
        (doseq [prediction (-> C meta :predictions)]
          (swap! predicts conj prediction))
        (doseq [[idx gold prediction fv-diff] C]
          (let [step-size (get-step-size weight gold prediction fv-diff)
                diff (->> fv-diff
                          (map (fn [[k v]] [k (* step-size v)])))
                number-of-cum-examples (+ (* iter n) idx)]
            (update-weight! weight diff 1.0)
            (update-weight! cum-weight diff number-of-cum-examples)))))
    (->> [iter
          (get-dependency-accuracy gold-sentences @predicts)
          (get-complete-accuracy gold-sentences @predicts)
          (get-labeled-dependency-accuracy gold-sentences @predicts)
          (get-labeled-complete-accuracy gold-sentences @predicts)]
         (clojure.string/join ", ")
       (println))))
