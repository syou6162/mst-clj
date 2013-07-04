(ns mst-clj.sentence
  (:import [mst_clj.word Word])
  (:use [mst-clj.feature :only (*update-feature-id?* get-fv)]))

(deftype Sentence [words edge-fvs]
  clojure.lang.Seqable
  (seq [this] (seq (.words this)))

  clojure.lang.Counted
  (count [this] (count (.words this)))

  clojure.lang.IFn
  (invoke [this idx] (get (.words this) idx))
  
  clojure.lang.Associative
  (assoc [this k v] (Sentence. (assoc (.words this) k v) (.edge-fvs this)))
  (valAt [this idx] (nth (.words this) idx nil)))

(defn make-training-data [words]
  (let [n (count words)
        init-edge-fvs (vec (repeat n (vec (repeat n (int-array 0)))))
        edge-fvs (->> (range 1 n)
                      (reduce (fn [result i]
                                (let [j (.head ^Word (nth words i))
                                      fv (get-fv words i j)]
                                  (assoc-in result [i j])))
                              init-edge-fvs))]
    (->> (for [i (range n), j (range 1 n)
               :when (and (not= i j)
                          (not (= j (.head ^Word (nth words i)))))]
           [i j])
         (reduce (fn [result [i j]]
                   (binding [*update-feature-id?* false]
                     (assoc-in result [i j] (get-fv words i j))))
                 edge-fvs)
         (Sentence. words))))

(defn make-test-data [words]
  (let [n (count words)
        init-edge-fvs (vec (repeat n (vec (repeat n (int-array 0)))))
        edge-fvs (reduce (fn [result [i j]]
                           (binding [*update-feature-id?* false]
                             (assoc-in result [i j] (get-fv words i j))))
                         init-edge-fvs
                         (for [i (range n), j (range 1 n)
                               :when (not= i j)]
                           [i j]))]
    (Sentence. words edge-fvs)))
