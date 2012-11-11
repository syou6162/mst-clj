(ns mst-clj.io
  (:use [mst-clj.word])
  (:require [mst-clj.sentence :as sentence])
  (:use [clojure.string :only (split)]))

(def Root :root)

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
       (map-indexed #(vector %1 %2))
       (pmap (fn [[sent-idx lines]]
               (let [[words pos-tags labels heads]
                     (map (fn [line]
                            (map clojure.string/lower-case (split line #"\t")))
                          (split lines #"\n"))
                     words (vec (map (fn [w pos-tag idx head]
                                       (struct word w pos-tag
                                               idx head))
                                     (vec (cons Root words))
                                     (vec (cons Root pos-tags))
                                     (vec (range (inc (count words))))
                                     (vec (cons -1 (map
                                                    #(Integer/parseInt %)
                                                    heads)))))]
                 (do
                   (binding [*out* *err*]
                     (print ".")
                     (flush))
                   (sentence/make words)))))
       (vec)))
