(ns mst-clj.io
  (:import [mst_clj.word Word])
  (:use [mst-clj.feature :only (get-fv)])
  (:use [clj-utils.random :only (shuffle-with-random)])
  (:use [clj-utils.time :only (easily-understandable-time)])
  (:require [mst-clj.word :as word])
  (:require [mst-clj.sentence :as sentence])
  (:use [clojure.string :only (split)]))

(def *root-surface* "ROOT-SURFACE")
(def *root-pos-tag* "ROOT-POS-TAG")

(defn lines-to-words [lines]
  (let [[words pos-tags labels heads] (->> (split lines #"\n")
                                           (mapv #(split % #"\t")))
        words (vec (map (fn [w pos-tag idx head]
                          (word/make w pos-tag idx head))
                        (vec (cons *root-surface* words))
                        (vec (cons *root-pos-tag* pos-tags))
                        (vec (range (inc (count words))))
                        (vec (cons -1 (map
                                       #(Integer/parseInt %)
                                       heads)))))]
    words))

(defn my-pmap
  ([num-of-threads f coll]
     (let [n (count coll)]
       (->> coll
            (partition-all (/ n num-of-threads))
            (pmap (fn [chuck] (mapv f chuck)))
            (reduce into []))))
  ([f coll]
     (my-pmap
      (+ 2 (.. Runtime getRuntime availableProcessors))
      f
      coll)))

(defn read-mst-format-file [filename]
  "Read correct parsed sentences from mst formal file.
   File format is as follows:

   ms.     haag    plays   elianti .
   NNP     NNP     VBZ     NNP     .
   DEP     NP-SBJ  ROOT    NP-OBJ  DEP
   2       3       0       3       3

   the     luxury  auto    maker   last    year    sold    1,214   cars    in      the     u.s.
   DT      NN      NN      NN      JJ      NN      VBD     CD      NNS     IN      DT      NNP
   DEP     DEP     DEP     NP-SBJ  DEP     NP      ROOT    DEP     NP-OBJ  PP      DEP     NP
   4       4       4       7       6       7       0       9       7       7       12      10"
  (binding [*out* *err*]
    (println "Creating Instances..."))
  (let [words-vec (->> (split (slurp filename) #"\n\n")
                       (shuffle-with-random)
                       (map lines-to-words))]
    (doseq [words words-vec
            j (range 1 (count words))]
      (let [i (.head ^Word (nth words j))]
        (get-fv words i j)))
    (->> words-vec
         (my-pmap
          (fn [words]
            (binding [*out* *err*] (print ".") (flush))
            (println (count words))
            (easily-understandable-time
             (sentence/make words))))
         (into []))))

(defn read-gold-sentences [filename]
  (->> (split (slurp filename) #"\n\n")
       (map lines-to-words)
       (my-pmap (fn [words]
                  (binding [*out* *err*] (print ".") (flush))
                  (sentence/make words)))
       (into [])))
