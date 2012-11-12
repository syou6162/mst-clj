(ns mst-clj.feature
  (:use [clj-utils.mapping]))

(def-obj-and-id-mapping feature)
(defstruct feature :type :str)
(def feature-names (atom []))

(defmacro def-feature-fn [feature-name args & body]
  `(let [name# (defn ~feature-name ~args ~@body)]
     (swap! feature-names conj name#)))

(defmacro def-conjunctive-feature-fn [& fs-list]
  (let [fs (vec fs-list)
        feature-name (symbol (apply str (interpose "-and-" fs)))]
    `(def-feature-fn ~feature-name [sentence# i# j#]
       (let [result# (->> ~fs
                          (map (fn [f#] (f# sentence# i# j#)))
                          (interpose "-and-")
                          (apply str))]
         result#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Uni-gram Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-feature-fn p-word [sentence i j]
  (get-in sentence [i :surface]))

(def-feature-fn p-pos [sentence i j]
  (get-in sentence [i :pos-tag]))

(def-conjunctive-feature-fn p-word p-pos)

(def-feature-fn c-word [sentence i j]
  (get-in sentence [j :surface]))

(def-feature-fn c-pos [sentence i j]
  (get-in sentence [j :pos-tag]))

(def-conjunctive-feature-fn c-word c-pos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Bi-gram Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-conjunctive-feature-fn p-word p-pos c-word c-pos)
(def-conjunctive-feature-fn p-pos c-word c-pos)
(def-conjunctive-feature-fn p-word c-word c-pos)
(def-conjunctive-feature-fn p-word p-pos c-pos)
(def-conjunctive-feature-fn p-word p-pos c-word)
(def-conjunctive-feature-fn p-word c-word)
(def-conjunctive-feature-fn p-pos c-pos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In Between POS Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Surrounding Word POS Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-feature-fn p-plus-word [sentence i j]
  (get-in sentence [(inc i) :surface]))

(def-feature-fn p-plus-pos [sentence i j]
  (get-in sentence [(inc i) :pos-tag]))

(def-feature-fn p-minus-word [sentence i j]
  (get-in sentence [(dec i) :surface]))

(def-feature-fn p-minus-pos [sentence i j]
  (get-in sentence [(dec i) :pos-tag]))

;;;;;

(def-feature-fn c-plus-word [sentence i j]
  (get-in sentence [(inc j) :surface]))

(def-feature-fn c-plus-pos [sentence i j]
  (get-in sentence [(inc j) :pos-tag]))

(def-feature-fn c-minus-word [sentence i j]
  (get-in sentence [(dec j) :surface]))

(def-feature-fn c-minus-pos [sentence i j]
  (get-in sentence [(dec j) :pos-tag]))

;;;;;

(def-conjunctive-feature-fn p-pos p-plus-pos c-minus-pos c-pos)
(def-conjunctive-feature-fn p-minus-pos p-pos c-minus-pos c-pos)
(def-conjunctive-feature-fn p-pos p-plus-pos c-pos c-plus-pos)
(def-conjunctive-feature-fn p-minus-pos p-pos c-pos c-plus-pos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Distance Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-feature-fn distance-1-feature [sentence i j]
  (str (= 1 (Math/abs (- i j)))))

(def-feature-fn distance-2-feature [sentence i j]
  (str (= 2 (Math/abs (- i j)))))

(def-feature-fn distance-3-feature [sentence i j]
  (str (= 3 (Math/abs (- i j)))))

(def-feature-fn distance-4-beyond-feature [sentence i j]
  (str (<= 4 (Math/abs (- i j)))))

(defn get-fv [sentence i j]
  (let [fv (->> (seq @feature-names)
                (map (fn [feature-fn]
                       (struct feature (str feature-fn)
                               (feature-fn sentence i j))))
                (flatten)
                (filter (fn [fv] (not (nil? (:str fv))))))]
    (->> fv
         (map feature-to-id)
         (into (vector-of :int))
         (vec))))
