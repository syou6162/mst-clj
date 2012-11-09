(ns mst-clj.feature
  (:use [clj-utils.mapping]))

(def-obj-and-id-mapping feature)
(defstruct feature :type :str)
(def feature-names (atom []))

(defmacro def-feature-fn [feature-name args & body]
  `(let [name# (defn ~feature-name ~args ~@body)]
     (swap! feature-names conj name#)))

(def-feature-fn surface-to-surface-feature
  [sentence i j]
  (let [wi (:surface (nth sentence i))
        wj (:surface (nth sentence j))]
    (struct feature
            'surface-to-surface-feature
            (str wi "-AND-" wj))))

(def-feature-fn surface-to-pos-feature
  [sentence i j]
  (let [wi (:surface (nth sentence i))
        pj (:pos-tag (nth sentence j))]
    (struct feature
            'surface-to-pos-feature
            (str wi "-AND-" pj))))

(def-feature-fn pos-to-surface-feature
  [sentence i j]
  (let [pi (:pos-tag (nth sentence i))
        wj (:surface (nth sentence j))]
    (struct feature
            'pos-to-surface-feature
            (str pi "-AND-" wj))))

(def-feature-fn pos-to-pos-feature
  [sentence i j]
  (let [pi (:pos-tag (nth sentence i))
        pj (:pos-tag (nth sentence j))]
    (struct feature
            'pos-to-pos-feature
            (str pi "-AND-" pj))))
