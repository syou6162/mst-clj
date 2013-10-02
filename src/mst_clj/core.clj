(ns mst-clj.core
  (:use [mst-clj.eisner :only (eisner)])
  (:use [mst-clj.perceptron :only (get-averaged-weight)])
  (:use [mst-clj.minibatch :only (*number-of-mini-batches* minibatch-update-weight)])
  (:use [mst-clj.io :only (read-mst-format-file read-gold-sentences)])
  (:use [mst-clj.evaluation
         :only (get-dependency-accuracy get-complete-accuracy
                get-labeled-dependency-accuracy get-labeled-complete-accuracy)])
  (:require [mst-clj.sentence :as sentence])
  (:require [mst-clj.feature :as feature])
  (:require [mst-clj.label :as label])
  (:use [clj-utils.time :only (easily-understandable-time)])
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
           ["--feature-to-id-filename" "File name of the feature2id mapping" :default "feature-to-id.bin"]
           ["--label-to-id-filename" "File name of the label2id mapping" :default "label-to-id.bin"]))

(defn make-decoder [weight] (partial eisner weight))

(defn train-mode [opts]
  (let [training-sentences (read-mst-format-file (:training-filename opts))
        n (count training-sentences)
        dev-sentences (read-gold-sentences (:dev-filename opts))
        weight-dim (inc (feature/get-max-feature-id))
        weight (double-array weight-dim)
        cum-weight (double-array weight-dim)]
    (label/save-label-to-id (:label-to-id-filename opts))
    (feature/save-feature-to-id (:feature-to-id-filename opts))
    (feature/clear-feature-mapping!)
    (doseq [iter (range 1 (inc (:max-iter opts)))]
      (binding [*number-of-mini-batches* (if (:mini-batch opts)
                                           *number-of-mini-batches* 1)]
        (minibatch-update-weight iter weight cum-weight training-sentences))
      (serialize (get-averaged-weight (* iter n) weight cum-weight)
                 (str iter "-" (:model-filename opts)))
      (let [decode (make-decoder
                    (get-averaged-weight (* iter n) weight cum-weight))
            [golds predictions] (let [golds dev-sentences
                                      predictions (mapv decode dev-sentences)]
                                  [golds predictions])]
        (binding [*out* *err*]
          (->> [iter
                (get-dependency-accuracy golds predictions)
                (get-complete-accuracy golds predictions)
                (get-labeled-dependency-accuracy golds predictions)
                (get-labeled-complete-accuracy golds predictions)]
               (clojure.string/join ", ")
               (println)))))
    (serialize (get-averaged-weight (* (:max-iter opts) n) weight cum-weight)
               (:model-filename opts))))

(defn eval-mode [opts]
  (binding [*out* *err*]
    (label/load-label-to-id! (:label-to-id-filename opts))
    (println (str "Started reading " (:feature-to-id-filename opts)))
    (time (feature/load-feature-to-id! (:feature-to-id-filename opts)))
    (println (str "Finished reading " (:feature-to-id-filename opts))))
  (let [weight (deserialize (:model-filename opts))
        decode (make-decoder weight)
        [golds predictions] (easily-understandable-time
                             (let [golds (read-gold-sentences (:test-filename opts))
                                   predictions (mapv decode golds)]
                               [golds predictions]))]
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
