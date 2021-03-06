(ns mst-clj.feature
  (:use [mst-clj.mapping :only (def-obj-and-id-mapping)])
  (:require [clojure.core.reducers :as r])
  (:import [mst_clj.word Word]))

(def-obj-and-id-mapping feature)

(defmacro def-conjunctive-feature-fn [& fs-list]
  (let [fs (vec fs-list)
        feature-name (symbol (clojure.string/join "-and-" fs))]
    `(defn ~feature-name [sentence# i# j#]
       (let [tmp# (map (fn [f#] (f# sentence# i# j#)) ~fs)]
         (if (every? #(not (nil? %)) tmp#)
           (clojure.string/join \& tmp#))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Uni-gram Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def unigram-feature
  [(defn p-word [sentence ^long i ^long j]
     (.surface ^Word (nth sentence i)))

   (defn p-word5 [sentence ^long i ^long j]
     (.lemma ^Word (nth sentence i)))

   (defn p-pos [sentence ^long i ^long j]
     (.pos-tag ^Word (nth sentence i)))

   (defn p-cpos [sentence ^long i ^long j]
     (.cpos-tag ^Word (nth sentence i)))

   (def-conjunctive-feature-fn p-word p-pos)
   (def-conjunctive-feature-fn p-word p-cpos)
   (def-conjunctive-feature-fn p-word5 p-pos)
   (def-conjunctive-feature-fn p-word5 p-cpos)

   (defn c-word [sentence ^long i ^long j]
     (.surface ^Word (nth sentence j)))

   (defn c-word5 [sentence ^long i ^long j]
     (.lemma ^Word (nth sentence j)))

   (defn c-pos [sentence ^long i ^long j]
     (.pos-tag ^Word (nth sentence j)))

   (defn c-cpos [sentence ^long i ^long j]
     (.cpos-tag ^Word (nth sentence j)))

   (def-conjunctive-feature-fn c-word c-pos)
   (def-conjunctive-feature-fn c-word c-cpos)
   (def-conjunctive-feature-fn c-word5 c-pos)
   (def-conjunctive-feature-fn c-word5 c-cpos)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Bi-gram Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def bigram-features
  [(def-conjunctive-feature-fn p-word p-pos c-word c-pos)
   (def-conjunctive-feature-fn p-word5 p-pos c-word c-pos)
   (def-conjunctive-feature-fn p-word p-pos c-word5 c-pos)
   (def-conjunctive-feature-fn p-word5 p-pos c-word5 c-pos)

   (def-conjunctive-feature-fn p-pos c-word c-pos)
   (def-conjunctive-feature-fn p-pos c-word5 c-pos)

   (def-conjunctive-feature-fn p-word c-word c-pos)
   (def-conjunctive-feature-fn p-word5 c-word c-pos)
   (def-conjunctive-feature-fn p-word c-word5 c-pos)
   (def-conjunctive-feature-fn p-word5 c-word5 c-pos)

   (def-conjunctive-feature-fn p-word p-pos c-pos)
   (def-conjunctive-feature-fn p-word5 p-pos c-pos)

   (def-conjunctive-feature-fn p-word p-pos c-word)
   (def-conjunctive-feature-fn p-word5 p-pos c-word)
   (def-conjunctive-feature-fn p-word p-pos c-word5)
   (def-conjunctive-feature-fn p-word5 p-pos c-word5)

   (def-conjunctive-feature-fn p-word c-word)
   (def-conjunctive-feature-fn p-word5 c-word)
   (def-conjunctive-feature-fn p-word c-word5)
   (def-conjunctive-feature-fn p-word5 c-word5)

   (def-conjunctive-feature-fn p-pos c-pos)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In Between POS Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn between-features [sentence ^long i ^long j]
  (let [i-pos (if (< (inc i) (count sentence))
                (.pos-tag ^Word (nth sentence i)))
        j-pos (if (< (inc j) (count sentence))
                (.pos-tag ^Word (nth sentence j)))]
    (->> (range (inc (min i j)) (max i j))
         (map (fn [mid-idx] (.pos-tag ^Word (nth sentence mid-idx))))
         (set)
         (vec)
         (reduce
          (fn [result mid-pos]
            (conj result (str i-pos \& mid-pos \& j-pos)))
          []))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Surrounding Word POS Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p-plus-pos [sentence ^long i ^long j]
  (if (< (inc i) (count sentence))
    (.pos-tag ^Word (nth sentence (inc i)))))
(defn p-minus-pos [sentence ^long i ^long j]
  (if (not (neg? (dec i)))
    (.pos-tag ^Word (nth sentence (dec i)))))
(defn c-plus-pos [sentence ^long i ^long j]
  (if (< (inc j) (count sentence))
    (.pos-tag ^Word (nth sentence (inc j)))))
(defn c-minus-pos [sentence ^long i ^long j]
  (if (not (neg? (dec j)))
    (.pos-tag ^Word (nth sentence (dec j)))))

(def surrounding-word-pos-features
  [(def-conjunctive-feature-fn p-pos p-plus-pos c-minus-pos c-pos)
   (def-conjunctive-feature-fn p-pos c-minus-pos c-pos)
   (def-conjunctive-feature-fn p-pos p-plus-pos c-pos)

   (def-conjunctive-feature-fn p-minus-pos p-pos c-minus-pos c-pos)
   (def-conjunctive-feature-fn p-minus-pos p-pos c-pos)

   (def-conjunctive-feature-fn p-pos p-plus-pos c-pos c-plus-pos)
   (def-conjunctive-feature-fn p-pos c-pos c-plus-pos)

   (def-conjunctive-feature-fn p-minus-pos p-pos c-pos c-plus-pos)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direction and Distance Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn direction-and-distance-feature [sentence ^long i ^long j]
  (let [direction (if (< i j) \l \r)
        dist (Math/abs (int (- i j)))
        dist-flag (cond (= dist 1) 1
                        (= dist 2) 2
                        (= dist 3) 3
                        (= dist 4) 4
                        (= dist 5) 5
                        (and (<= 5 dist) (>= 10 dist)) 10
                        :else 11)]
    (str direction \& dist-flag)))

(def all-basic-features
  (->> [unigram-feature bigram-features
        surrounding-word-pos-features]
       (reduce into [])))

(defn get-fv [sentence i j]
  (let [dir-dist-feature (direction-and-distance-feature sentence i j)
        all-basic-features (->> all-basic-features
                                (r/map (fn [feature-fn]
                                         (if-let [result (feature-fn sentence i j)]
                                           (str (-> feature-fn meta :name) \& result))))
                                (r/remove nil?)
                                (r/fold (r/monoid into vector) conj))
        bet-fv (r/map #(str "b&" %) (between-features sentence i j))]
    (->> (into all-basic-features bet-fv)
         (r/map (fn [f] [f (str dir-dist-feature \& f)]))
         (r/flatten)
         (r/map feature-to-id)
         (r/remove nil?)
         (r/fold (r/monoid into vector) conj))))
