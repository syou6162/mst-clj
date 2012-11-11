(ns mst-clj.core
  (:use [mst-clj.eisner])
  (:use [mst-clj.perceptron])
  (:require [mst-clj.feature :as feature])
  (:use [mst-clj.evaluation])
  (:use [mst-clj.io])
  (:use [clj-utils.core :only (split-with-ratio)])
  (:use [clj-utils.io :only (serialize deserialize)]))

(require '[clojure.tools.cli :as cli])

(defn- get-cli-opts [args]
  (cli/cli args
           ["-h" "--help" "Show help" :default false :flag true]
           ["--mode" "(train|test|eval)"]
           ["--filename" "File name for (training|test|eval)" :default "/Users/yasuhisa/Desktop/simple_shift_reduce_parsing/wsj_02_21_mst.txt"]
           ["--model-filename" "File name of the (saved|load) model" :default "parsing.model"]
           ["--max-iter" "Number of maximum iterations" :default 10 :parse-fn #(Integer. %)]
           ["--feature-to-id-filename" "File name of the feature2id mapping" :default "feature-to-id.bin"]))

(defn parse-fn [weight]
  (fn [sentence] (do (binding [*out* *err*]
                       (print ".")
                       (flush))
                     (eisner sentence weight))))

(defn train-mode [filename max-iter model-filename feature-to-id-filename]
  (let [sentences (read-mst-format-file filename)
        _ (feature/save-feature-to-id feature-to-id-filename)
        _ (feature/clear-feature-mapping!)]
    (loop [iter 0, weight {}]
      (if (= iter max-iter)
        (serialize (with-meta
                     weight
                     {:filename filename
                      :num-of-training-sentences (count sentences)
                      :max-iter max-iter})
                   model-filename)
        (do
          (binding [*out* *err*]
            (println (str "\nIteration: " iter)))
          (recur (inc iter)
                 (loop [sent-idx 0, weight weight]
                   (if (= sent-idx (count sentences))
                     weight
                     (do
                       (binding [*out* *err*]
                         (print ".")
                         (flush))
                       (recur (inc sent-idx)
                              (update-weight weight
                                             (nth sentences sent-idx)
                                             (eisner (nth sentences sent-idx) weight))))))))))))

(defn eval-mode [filename model-filename feature-to-id-filename]
  (let [_ (feature/load-feature-to-id feature-to-id-filename)
        weight (deserialize model-filename)
        golds (read-mst-format-file filename)
        parse (parse-fn weight)
        predictions (doall (pmap parse golds))]
    (binding [*out* *err*]
      (println "\nNumber of features: " (count weight))
      (println (get-dependency-accuracy golds predictions))
      (println (get-complete-accuracy golds predictions)))))

(defn -main [& args]
  (let [[options rest-args banner] (get-cli-opts args)]
    (when (:help options)
      (println banner)
      (System/exit 0))
    (cond (= "train" (:mode options)) (train-mode (:filename options) (:max-iter options)
                                                  (:model-filename options) (:feature-to-id-filename options))
          (= "eval" (:mode options)) (eval-mode (:filename options) (:model-filename options) (:feature-to-id-filename options))
          :else nil))
  (shutdown-agents))
