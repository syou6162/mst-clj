(ns mst-clj.feature
  (:use [clojure.math.combinatorics])
  (:use [mst-clj.mapping]))

(def-obj-and-id-mapping feature)
(defstruct feature :type :str)
(def feature-names (atom []))

(defmacro def-feature-fn
  ([feature-name] `(swap! feature-names conj ~feature-name))
  ([feature-name args & body]
     `(let [name# (defn ~feature-name ~args ~@body)]
        (swap! feature-names conj name#))))

(defmacro def-conjunctive-feature-fn [& fs-list]
  (let [fs (vec fs-list)
        feature-name (symbol (apply str (interpose "-and-" fs)))]
    `(defn ~feature-name [sentence# i# j#]
       (let [strs# (map (fn [f#] (f# sentence# i# j#)) ~fs)
             result# (if (every? #(not (nil? %)) strs#)
                       (->> strs# (interpose "-and-") (apply str))
                       nil)]
         result#))))

(defmacro def-conjunctive-features-fn [features-a features-b]
  (list* 'do (map (fn [[a b]] `(def-feature-fn (def-conjunctive-feature-fn ~a ~b)))
                  (cartesian-product features-a features-b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Uni-gram Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def unigram-feature
  [(defn p-word [sentence i j] (get-in sentence [i :surface]))
   (defn p-word5 [sentence i j] (let [surface (get-in sentence [i :surface])]
                                  (if (and (not (keyword? surface)) (< 5 (count surface)))
                                    (subs surface 0 5))))
   (defn p-pos [sentence i j] (get-in sentence [i :pos-tag]))
   (def-conjunctive-feature-fn p-word p-pos)
   (def-conjunctive-feature-fn p-word5 p-pos)

   (defn c-word [sentence i j] (get-in sentence [j :surface]))
   (defn c-word5 [sentence i j] (let [surface (get-in sentence [j :surface])]
                                  (if (and (not (keyword? surface)) (< 5 (count surface)))
                                    (subs surface 0 5))))
   (defn c-pos [sentence i j] (get-in sentence [j :pos-tag]))
   (def-conjunctive-feature-fn c-word c-pos)
   (def-conjunctive-feature-fn c-word5 c-pos)])

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Surrounding Word POS Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p-plus-pos [sentence i j] (get-in sentence [(inc i) :pos-tag]))
(defn p-minus-pos [sentence i j] (get-in sentence [(dec i) :pos-tag]))
(defn c-plus-pos [sentence i j] (get-in sentence [(inc j) :pos-tag]))
(defn c-minus-pos [sentence i j] (get-in sentence [(dec j) :pos-tag]))

(def surrounding-word-pos-features
  [(def-conjunctive-feature-fn p-pos p-plus-pos c-minus-pos c-pos)
   (def-conjunctive-feature-fn p-minus-pos p-pos c-minus-pos c-pos)
   (def-conjunctive-feature-fn p-pos p-plus-pos c-pos c-plus-pos)
   (def-conjunctive-feature-fn p-minus-pos p-pos c-pos c-plus-pos)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direction and Distance Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn direction-and-distance-feature [sentence i j]
  (let [direction (if (< i j) :left :right)
        dist (Math/abs (- i j))
        dist-flag (cond (= dist 1) :1
                        (and (<= 2 dist) (>= 5 dist)) :2-5
                        :else :6)]
    (str direction dist-flag)))

(def all-basic-features
  (map (comp :name meta)
       (concat unigram-feature bigram-features surrounding-word-pos-features)))

(defmacro def-all-features []
  (list* `do (map
              (fn [f] `(def-feature-fn (def-conjunctive-feature-fn direction-and-distance-feature ~f)))
              all-basic-features)))

(def-all-features)

(defn get-fv [sentence i j]
  (let [fv (->> (seq @feature-names)
                (map (fn [feature-fn]
                       (struct feature (str feature-fn) (feature-fn sentence i j))))
                (filter (fn [fv] (not (nil? (:str fv))))))]
    (->> fv
         (map (fn [feature] (str (:type feature) (:str feature))))
         (map feature-to-id)
         (into-array Integer/TYPE))))
