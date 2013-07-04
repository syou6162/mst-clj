(ns mst-clj.io
  (:import [mst_clj.word Word])
  (:require [mst-clj.sentence :as sentence])
  (:use [clojure.string :only (split)]))

(def Root :root)

(defn lines-to-words [lines]
  (let [[words pos-tags labels heads] (->> (split lines #"\n")
                                           (mapv (fn [line]
                                                   (->> (split line #"\t")
                                                        (mapv clojure.string/lower-case)))))
        words (vec (map (fn [w pos-tag idx head]
                          (Word. w pos-tag idx head))
                        (vec (cons Root words))
                        (vec (cons Root pos-tags))
                        (vec (range (inc (count words))))
                        (vec (cons -1 (map
                                       #(Integer/parseInt %)
                                       heads)))))]
    words))

(defn lines-to-sentence [lines]
  (binding [*out* *err*]
    (print ".")
    (flush))
  (sentence/make (lines-to-words lines)))

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
  (->> (split (slurp filename) #"\n\n")
       (map lines-to-words)
       (mapv sentence/make-training-data)))

(defn read-gold-sentences [filename]
  (->> (split (slurp filename) #"\n\n")
       (map lines-to-words)
       (mapv sentence/make-test-data)))
