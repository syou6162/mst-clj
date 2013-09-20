(ns mst-clj.core
  (:use [mst-clj.eisner :only (eisner eisner-for-training)])
  (:use [mst-clj.minibatch :only (minibatch-update-weight)])
  (:use [mst-clj.io])
  (:use [mst-clj.evaluation])
  (:require [mst-clj.sentence :as sentence])
  (:require [mst-clj.feature :as feature])
  (:require [mst-clj.perceptron :as perceptron])
  (:use [clj-utils.core :only (split-with-ratio)])
  (:use [clj-utils.io :only (serialize deserialize)])
  (:gen-class))

(require '[clojure.tools.cli :as cli])

(defn- get-cli-opts [args]
  (cli/cli args
           ["-h" "--help" "Show help" :default false :flag true]
           ["--mode" "(training|test|eval)"]
           ["--mini-batch" "Use mini-batch to parallel training data" :default false :flag true]
           ["--training-filename" "File name for training" :default "train.txt"]
           ["--dev-filename" "File name for dev" :default "dev.txt"]
           ["--test-filename" "File name for test" :default "test.txt"]
           ["--model-filename" "File name of the (saved|load) model" :default "parsing.model"]
           ["--max-iter" "Number of maximum iterations" :default 10 :parse-fn #(Integer. %)]
           ["--feature-to-id-filename" "File name of the feature2id mapping" :default "feature-to-id.bin"]))

(defn parse-fn [weight]
  (fn [sentence] (do (binding [*out* *err*]
                       (print ".")
                       (flush))
                     (eisner sentence weight))))

(defn calc-accuracy [iter weight training-sentences dev-sentences]
  (let [training-predictions (map #(eisner % weight) training-sentences)
        dev-predictions (map #(eisner % weight) dev-sentences)]
    (->> [iter
          (get-dependency-accuracy training-sentences training-predictions)
          (get-complete-accuracy training-sentences training-predictions)
          (get-dependency-accuracy dev-sentences dev-predictions)
          (get-complete-accuracy dev-sentences dev-predictions)]
         (clojure.string/join ", ")
         (println))))

(defn train-mode [opts]
  (let [training-sentences (read-mst-format-file (:training-filename opts))
        dev-sentences (read-gold-sentences (:dev-filename opts))
        weight-dim (inc (feature/get-max-feature-id))]
    (feature/save-feature-to-id (:feature-to-id-filename opts))
    (feature/clear-feature-mapping!)
    (loop [iter 0,
           weight (double-array weight-dim)
           cum-weight (double-array weight-dim)]
      (if (= iter (:max-iter opts))
        (serialize (perceptron/averaged-weight cum-weight (* iter (count training-sentences)))
                   (:model-filename opts))
        (let [[[new-weight cum-weight] _ _] (pvalues
                                             ((if (:mini-batch opts) minibatch-update-weight update-weight)
                                              weight cum-weight training-sentences)
                                             (calc-accuracy iter weight training-sentences dev-sentences)
                                             (-> cum-weight
                                                 (perceptron/averaged-weight (* iter (count training-sentences)))
                                                 (serialize (str iter "-" (:model-filename opts)))))]
          (recur (inc iter) new-weight cum-weight))))))

(defn eval-mode [opts]
  (let [_ (binding [*out* *err*] (println (str "Started reading " (:feature-to-id-filename opts))))
        _ (time (feature/load-feature-to-id! (:feature-to-id-filename opts)))
        _ (binding [*out* *err*] (println (str "Finished reading " (:feature-to-id-filename opts))))
        weight (deserialize (:model-filename opts))
        _ (binding [*out* *err*] (println "Started reading gold sentences..."))
        golds (read-gold-sentences (:test-filename opts))
        _ (binding [*out* *err*] (println "Finished reading gold sentences..."))
        parse (parse-fn weight)
        predictions (mapv parse golds)]
    (binding [*out* *err*]
      (println "\nNumber of features: " (count weight))
      (println (get-dependency-accuracy golds predictions))
      (println (get-complete-accuracy golds predictions)))))

(defn -main [& args]
  (let [[options rest-args banner] (get-cli-opts args)]
    (when (:help options)
      (println banner)
      (System/exit 0))
    (cond (= "training" (:mode options)) (train-mode options)
          (= "eval" (:mode options)) (eval-mode options)
          :else nil))
  (shutdown-agents))
