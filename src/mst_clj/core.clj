(ns mst-clj.core
  (:use [mst-clj.eisner])
  (:use [mst-clj.perceptron])
  (:use [mst-clj.feature])
  (:use [mst-clj.evaluation])
  (:use [mst-clj.io])
  (:use [clj-utils.core :only (split-with-ratio)]))

(defn parse-fn [weight]
  (fn [sentence] (eisner sentence weight)))

(defn train [sentences test-sentences max-iter]
  (loop [iter 0
         weight {}]
    (if (= iter max-iter)
      nil
      (recur (inc iter)
             (loop [sent-idx 0
                    weight weight]
               (if (= sent-idx (count sentences))
                 (let [parse (parse-fn weight)
                       golds test-sentences
                       predictions (doall (pmap parse golds))]
                   (binding [*out* *err*]
                     (println "Number of features: " (count weight))
                     (println (get-dependency-accuracy golds predictions))
                     (println (get-complete-accuracy golds predictions)))
                   weight)
                 (do
                   (recur (inc sent-idx)
                          (update-weight
                           weight
                           (nth sentences sent-idx)
                           (eisner (nth sentences sent-idx) weight))))))))))

(defn -main [& args]
  (let [filename "/Users/yasuhisa/Desktop/simple_shift_reduce_parsing/wsj_02_21_mst.txt"
        ratio 0.9
        [sentences test-sentences] (split-with-ratio ratio (read-mst-format-file filename))]
    (train sentences test-sentences 100)))
