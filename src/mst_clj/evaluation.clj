(ns mst-clj.evaluation
  (:import [mst_clj.word Word])
  (:import [mst_clj.sentence Sentence]))

(defn get-dependency-accuracy' [comp-fn original-sentences parsed-sentences]
  (let [pairs (map (fn [[^Sentence original-sentence, ^Sentence parsed-sentence]]
                     (let [num-of-correct-heads (reduce + (map (fn [[gold predict]]
                                                                 (if (comp-fn gold predict) 1.0 0.0))
                                                               (map vector
                                                                    (rest (:words original-sentence))
                                                                    (rest (:words parsed-sentence)))))]
                       (vector num-of-correct-heads (dec (count (:words original-sentence))))))
                   (map vector original-sentences parsed-sentences))
        corrects (map first pairs)
        lengths (map second pairs)]
    (/ (reduce + corrects) (reduce + lengths))))

(defn same-head? [^Word gold ^Word predict]
  (= (.head gold) (.head predict)))
(defn same-head-and-label? [^Word gold ^Word predict]
  (and (= (.head gold) (.head predict))
       (= (.label gold) (.label predict))))

(def get-dependency-accuracy (partial get-dependency-accuracy' same-head?))
(def get-labeled-dependency-accuracy (partial get-dependency-accuracy' same-head-and-label?))

(defn get-complete-accuracy' [comp-fn original-sentences parsed-sentences]
  (assert (= (count original-sentences) (count parsed-sentences)))
  (let [num-of-sent (count original-sentences)
        num-of-complete (reduce
                         (fn [sum sent-idx]
                           (if (every? (fn [[gold predict]] (comp-fn gold predict))
                                       (map vector
                                            (:words (nth original-sentences sent-idx))
                                            (:words (nth parsed-sentences sent-idx))))
                             (+ 1.0 sum)
                             sum))
                         0.0
                         (range num-of-sent))]
    (/ num-of-complete num-of-sent)))

(def get-complete-accuracy (partial get-complete-accuracy' same-head?))
(def get-labeled-complete-accuracy (partial get-complete-accuracy' same-head-and-label?))
