(ns mst-clj.sentence
  (:use [mst-clj.word])
  (:use [mst-clj.feature]))

(deftype Sentence [words edge-fvs]
  clojure.lang.Seqable
  (seq [this] (seq (.words this)))

  clojure.lang.Counted
  (count [this] (count (.words this)))

  clojure.lang.IFn
  (invoke [this idx] (get (.words this) idx))
  
  clojure.lang.Associative
  (assoc [this k v] (assoc (.words this) k v))
  (valAt [this idx] (nth (.words this) idx nil)))

(defn make [words]
  (let [n (count words)
        init-edge-fvs (vec (repeat n (vec (repeat n []))))
        edge-fvs (reduce (fn [result [i j]]
                           (assoc-in result [i j] (get-fv words i j)))
                         init-edge-fvs
                         (for [i (range n), j (range n)
                               :when (not= i j)]
                           [i j]))]
    (Sentence. words edge-fvs)))
